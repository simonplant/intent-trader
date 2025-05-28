from datetime import datetime
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field, field_validator

from data.schemas import MarketDataSchema, OrderSchema, PositionSchema


class OrderParameters(BaseModel):
    """Parameters for order execution"""

    symbol: str
    side: str
    quantity: float
    order_type: str
    price: Optional[float] = None
    stop_price: Optional[float] = None
    time_in_force: str = "day"
    routing: str = "smart"
    notes: Optional[str] = None

    @field_validator("side")
    @classmethod
    def validate_side(cls, v):
        if v not in ["buy", "sell"]:
            raise ValueError("Side must be either 'buy' or 'sell'")
        return v

    @field_validator("order_type")
    @classmethod
    def validate_order_type(cls, v):
        if v not in ["market", "limit", "stop", "stop_limit"]:
            raise ValueError("Order type must be one of: market, limit, stop, stop_limit")
        return v

    @field_validator("time_in_force")
    @classmethod
    def validate_time_in_force(cls, v):
        if v not in ["day", "gtc", "ioc", "fok"]:
            raise ValueError("Time in force must be one of: day, gtc, ioc, fok")
        return v


class OrderManager:
    """Manager for order execution and tracking"""

    def __init__(self):
        self.orders: Dict[str, OrderSchema] = {}
        self.positions: Dict[str, PositionSchema] = {}
        self.market_data: Dict[str, MarketDataSchema] = {}
        self.order_history: List[OrderSchema] = []

    def update_market_data(self, market_data: Dict[str, MarketDataSchema]) -> None:
        """Update market data for order management"""
        self.market_data.update(market_data)

    def update_positions(self, positions: Dict[str, PositionSchema]) -> None:
        """Update positions for order management"""
        self.positions.update(positions)

    def place_order(self, params: OrderParameters) -> Dict[str, Any]:
        """Place a new order"""
        # Validate order parameters
        if params.symbol not in self.market_data:
            return {
                "status": "error",
                "message": f"No market data available for {params.symbol}",
            }

        # Create order
        order = OrderSchema(
            order_id=f"order_{len(self.order_history) + 1}",
            symbol=params.symbol,
            side=params.side,
            order_type=params.order_type,
            quantity=params.quantity,
            price=params.price,
            status="pending",
            timestamp=datetime.now(),
            metadata={
                "stop_price": params.stop_price,
                "time_in_force": params.time_in_force,
                "routing": params.routing,
                "notes": params.notes,
            },
        )

        # Store order
        self.orders[order.order_id] = order
        self.order_history.append(order)

        # Execute order based on type
        if params.order_type == "market":
            return self._execute_market_order(order)
        elif params.order_type == "limit":
            return self._execute_limit_order(order)
        elif params.order_type == "stop":
            return self._execute_stop_order(order)
        else:  # stop_limit
            return self._execute_stop_limit_order(order)

    def cancel_order(self, order_id: str) -> Dict[str, Any]:
        """Cancel an existing order"""
        if order_id not in self.orders:
            return {"status": "error", "message": f"Order {order_id} not found"}

        order = self.orders[order_id]
        if order.status not in ["pending", "partially_filled"]:
            return {
                "status": "error",
                "message": f"Cannot cancel order in {order.status} status",
            }

        # Update order status
        order.status = "cancelled"
        order.metadata["cancelled_at"] = datetime.now()

        return {
            "status": "success",
            "message": f"Order {order_id} cancelled",
            "data": order.dict(),
        }

    def get_order_status(self, order_id: str) -> Dict[str, Any]:
        """Get status of an order"""
        if order_id not in self.orders:
            return {"status": "error", "message": f"Order {order_id} not found"}

        return {"status": "success", "data": self.orders[order_id].dict()}

    def get_open_orders(self) -> Dict[str, Any]:
        """Get all open orders"""
        open_orders = {
            order_id: order.dict()
            for order_id, order in self.orders.items()
            if order.status in ["pending", "partially_filled"]
        }

        return {"status": "success", "data": open_orders}

    def _execute_market_order(self, order: OrderSchema) -> Dict[str, Any]:
        """Execute a market order"""
        market_data = self.market_data[order.symbol]

        # Simulate execution at current market price
        execution_price = market_data.price
        if order.side == "sell":
            execution_price *= 0.999  # Slight slippage for sells
        else:
            execution_price *= 1.001  # Slight slippage for buys

        # Update order status
        order.status = "filled"
        order.metadata["execution_price"] = execution_price
        order.metadata["executed_at"] = datetime.now()

        # Update position
        self._update_position(order, execution_price)

        return {
            "status": "success",
            "message": f"Market order executed at {execution_price}",
            "data": order.dict(),
        }

    def _execute_limit_order(self, order: OrderSchema) -> Dict[str, Any]:
        """Execute a limit order"""
        market_data = self.market_data[order.symbol]

        # Check if limit price is met
        if order.side == "buy" and market_data.price <= order.price:
            return self._execute_market_order(order)
        elif order.side == "sell" and market_data.price >= order.price:
            return self._execute_market_order(order)

        return {
            "status": "success",
            "message": "Limit order placed",
            "data": order.dict(),
        }

    def _execute_stop_order(self, order: OrderSchema) -> Dict[str, Any]:
        """Execute a stop order"""
        market_data = self.market_data[order.symbol]
        stop_price = order.metadata["stop_price"]

        # Check if stop price is met
        if order.side == "buy" and market_data.price >= stop_price:
            return self._execute_market_order(order)
        elif order.side == "sell" and market_data.price <= stop_price:
            return self._execute_market_order(order)

        return {
            "status": "success",
            "message": "Stop order placed",
            "data": order.dict(),
        }

    def _execute_stop_limit_order(self, order: OrderSchema) -> Dict[str, Any]:
        """Execute a stop-limit order"""
        market_data = self.market_data[order.symbol]
        stop_price = order.metadata["stop_price"]

        # Check if stop price is met
        if order.side == "buy" and market_data.price >= stop_price:
            return self._execute_limit_order(order)
        elif order.side == "sell" and market_data.price <= stop_price:
            return self._execute_limit_order(order)

        return {
            "status": "success",
            "message": "Stop-limit order placed",
            "data": order.dict(),
        }

    def _update_position(self, order: OrderSchema, execution_price: float) -> None:
        """Update position after order execution"""
        symbol = order.symbol
        quantity = order.quantity
        if order.side == "sell":
            quantity = -quantity

        if symbol in self.positions:
            position = self.positions[symbol]
            # Update existing position
            new_quantity = position.quantity + quantity
            if new_quantity == 0:
                del self.positions[symbol]
            else:
                position.quantity = new_quantity
                position.current_price = execution_price
                position.pnl = (execution_price - position.entry_price) * position.quantity
                position.pnl_percent = position.pnl / (position.entry_price * position.quantity)
        else:
            # Create new position
            if quantity > 0:
                self.positions[symbol] = PositionSchema(
                    position_id=f"position_{len(self.positions) + 1}",
                    symbol=symbol,
                    quantity=quantity,
                    entry_price=execution_price,
                    current_price=execution_price,
                    pnl=0.0,
                    pnl_percent=0.0,
                    timestamp=datetime.now(),
                    status="open",
                )
