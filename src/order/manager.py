"""
Order Management System - Core component for handling trading orders.
"""

import logging
from dataclasses import dataclass
from datetime import datetime, UTC
from enum import Enum
from typing import Any, Dict, List, Optional, Union
from uuid import UUID, uuid4

from pydantic import BaseModel, Field, field_validator

from data.schemas import MarketDataSchema, OrderSchema, PositionSchema

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class OrderStatus(Enum):
    """Enumeration of possible order statuses."""

    PENDING = "pending"
    SUBMITTED = "submitted"
    PARTIALLY_FILLED = "partially_filled"
    FILLED = "filled"
    CANCELED = "canceled"
    REJECTED = "rejected"
    EXPIRED = "expired"


class OrderType(Enum):
    """Enumeration of possible order types."""

    MARKET = "market"
    LIMIT = "limit"
    STOP = "stop"
    STOP_LIMIT = "stop_limit"


class OrderSide(Enum):
    """Enumeration of possible order sides."""

    BUY = "buy"
    SELL = "sell"


@dataclass
class Order:
    """Represents a trading order."""

    id: UUID
    symbol: str
    side: OrderSide
    type: OrderType
    quantity: float
    price: Optional[float] = None
    stop_price: Optional[float] = None
    status: OrderStatus = OrderStatus.PENDING
    created_at: datetime = datetime.now(UTC)
    updated_at: datetime = datetime.now(UTC)
    filled_quantity: float = 0.0
    average_fill_price: Optional[float] = None
    client_order_id: Optional[str] = None
    exchange_order_id: Optional[str] = None

    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "symbol": self.symbol,
            "side": self.side.value,
            "type": self.type.value,
            "quantity": self.quantity,
            "price": self.price,
            "status": self.status.value,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
        }


class OrderParameters(BaseModel):
    """Parameters for order execution"""

    symbol: str
    side: str
    quantity: float
    order_type: str
    price: Optional[float] = None
    stop_price: Optional[float] = None
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
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
            raise ValueError("Order type must be 'market', 'limit', 'stop', or 'stop_limit'")
        return v

    @field_validator("time_in_force")
    @classmethod
    def validate_time_in_force(cls, v):
        if v not in ["day", "gtc", "ioc", "fok"]:
            raise ValueError("Time in force must be one of: day, gtc, ioc, fok")
        return v

    @field_validator("stop_loss", "take_profit")
    @classmethod
    def validate_stop_loss_take_profit(cls, v):
        if v is not None and v <= 0:
            raise ValueError("Stop loss and take profit must be positive")
        return v

    class Config:
        extra = "allow"


class OrderManager:
    """Manages the creation, modification, and tracking of trading orders."""

    def __init__(self):
        """Initialize the OrderManager."""
        self._orders: Dict[UUID, Order] = {}
        self._order_book: Dict[str, List[Order]] = {}
        self.positions: Dict[str, float] = {}
        self.orders: Dict[str, OrderSchema] = {}
        self.market_data: Dict[str, MarketDataSchema] = {}
        self.order_history: List[OrderSchema] = []

    def create_order(
        self,
        symbol: str,
        side: Union[OrderSide, str],
        order_type: Union[OrderType, str],
        quantity: float,
        price: Optional[float] = None,
        stop_price: Optional[float] = None,
        client_order_id: Optional[str] = None,
    ) -> Order:
        """
        Create a new order.

        Args:
            symbol: Trading pair symbol
            side: Order side (buy/sell)
            order_type: Type of order (market/limit/stop)
            quantity: Order quantity
            price: Order price (required for limit orders)
            stop_price: Stop price (required for stop orders)
            client_order_id: Optional client-specific order ID

        Returns:
            Order: The created order object

        Raises:
            ValueError: If required parameters are missing or invalid
        """
        # Convert string inputs to enums if necessary
        if isinstance(side, str):
            side = OrderSide(side.lower())
        if isinstance(order_type, str):
            order_type = OrderType(order_type.lower())

        # Validate order parameters
        if order_type in (OrderType.LIMIT, OrderType.STOP_LIMIT) and price is None:
            raise ValueError(f"Price is required for {order_type.value} orders")
        if order_type in (OrderType.STOP, OrderType.STOP_LIMIT) and stop_price is None:
            raise ValueError(f"Stop price is required for {order_type.value} orders")

        # Create order
        order = Order(
            id=uuid4(),
            symbol=symbol,
            side=side,
            type=order_type,
            quantity=quantity,
            price=price,
            stop_price=stop_price,
            client_order_id=client_order_id,
        )

        # Store order
        self._orders[order.id] = order
        if symbol not in self._order_book:
            self._order_book[symbol] = []
        self._order_book[symbol].append(order)

        logger.info(f"Created order: {order.to_dict()}")
        return order

    def get_order(self, order_id: UUID) -> Optional[Order]:
        """
        Retrieve an order by its ID.

        Args:
            order_id: The UUID of the order

        Returns:
            Optional[Order]: The order if found, None otherwise
        """
        return self._orders.get(order_id)

    def get_orders_by_symbol(self, symbol: str) -> List[Order]:
        """
        Retrieve all orders for a specific symbol.

        Args:
            symbol: The trading pair symbol

        Returns:
            List[Order]: List of orders for the symbol
        """
        return self._order_book.get(symbol, [])

    def update_order_status(
        self,
        order_id: UUID,
        status: Union[OrderStatus, str],
        filled_quantity: Optional[float] = None,
        average_fill_price: Optional[float] = None,
        exchange_order_id: Optional[str] = None,
    ) -> Optional[Order]:
        """
        Update the status and details of an order.

        Args:
            order_id: The UUID of the order
            status: New order status
            filled_quantity: Updated filled quantity
            average_fill_price: Updated average fill price
            exchange_order_id: Exchange-specific order ID

        Returns:
            Optional[Order]: The updated order if found, None otherwise
        """
        order = self._orders.get(order_id)
        if not order:
            return None

        if isinstance(status, str):
            status = OrderStatus(status.lower())

        order.status = status
        order.updated_at = datetime.now(UTC)

        if filled_quantity is not None:
            order.filled_quantity = filled_quantity
        if average_fill_price is not None:
            order.average_fill_price = average_fill_price
        if exchange_order_id is not None:
            order.exchange_order_id = exchange_order_id

        logger.info(f"Updated order {order_id} status to {status.value}")
        return order

    def cancel_order(self, order_id: UUID) -> Optional[Order]:
        """
        Cancel an existing order.

        Args:
            order_id: The UUID of the order to cancel

        Returns:
            Optional[Order]: The canceled order if found, None otherwise
        """
        # Handle string order_id
        if isinstance(order_id, str):
            # Try to find the order by string ID in self.orders
            if order_id in self.orders:
                order = self.orders[order_id]
                if order.status not in ["pending", "partially_filled"]:
                    return {
                        "status": "error",
                        "message": f"Cannot cancel order in {order.status} status"
                    }
                
                order.status = "cancelled"
                order.metadata["cancelled_at"] = datetime.now(UTC)
                
                return {
                    "status": "success",
                    "message": f"Order {order_id} cancelled",
                    "data": order.model_dump()
                }
            else:
                return {"status": "error", "message": f"Order {order_id} not found"}
        
        # Original UUID-based logic
        order = self._orders.get(order_id)
        if not order:
            return None

        if order.status not in (
            OrderStatus.PENDING,
            OrderStatus.SUBMITTED,
            OrderStatus.PARTIALLY_FILLED,
        ):
            raise ValueError(f"Cannot cancel order in {order.status.value} status")

        order.status = OrderStatus.CANCELED
        order.updated_at = datetime.now(UTC)

        logger.info(f"Cancelled order {order_id}")
        return order

    def get_open_orders(self, symbol: Optional[str] = None) -> List[Order]:
        """
        Retrieve all open orders, optionally filtered by symbol.

        Args:
            symbol: Optional symbol to filter orders

        Returns:
            List[Order]: List of open orders
        """
        open_statuses = {
            OrderStatus.PENDING,
            OrderStatus.SUBMITTED,
            OrderStatus.PARTIALLY_FILLED,
        }

        if symbol:
            orders = self._order_book.get(symbol, [])
        else:
            orders = list(self._orders.values())

        return [order for order in orders if order.status in open_statuses]

    def get_position(self, symbol: str) -> float:
        """Get current position size for a symbol."""
        return self.positions.get(symbol, 0.0)

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
            timestamp=datetime.now(UTC),
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

    def get_order_status(self, order_id: str) -> Dict[str, Any]:
        """Get status of an order"""
        if order_id not in self.orders:
            return {"status": "error", "message": f"Order {order_id} not found"}

        return {"status": "success", "data": self.orders[order_id].model_dump()}

    def get_open_orders_dict(self) -> Dict[str, Any]:
        """Get all open orders as a dictionary response"""
        open_orders = {
            order_id: order.model_dump()
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
        order.metadata["executed_at"] = datetime.now(UTC)

        # Update position
        self._update_position(order, execution_price)

        return {
            "status": "success",
            "message": f"Market order executed at {execution_price}",
            "data": order.model_dump(),
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
            "data": order.model_dump(),
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
            "data": order.model_dump(),
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
            "data": order.model_dump(),
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
                    timestamp=datetime.now(UTC),
                    status="open",
                    stop_loss=order.metadata.get("stop_loss", 0.0),
                    take_profit=order.metadata.get("take_profit", 0.0),
                )


if __name__ == "__main__":
    # Test the order manager
    manager = OrderManager()

    # Create a market buy order
    order = manager.create_order(
        symbol="AAPL", side=OrderSide.BUY, order_type=OrderType.MARKET, quantity=100
    )
    print(f"Created order: {order.to_dict()}")

    # Update order status
    manager.update_order_status(order.id, OrderStatus.FILLED)
    print(f"Updated order: {order.to_dict()}")

    # Get open orders
    open_orders = manager.get_open_orders()
    print(f"Open orders: {len(open_orders)}")
