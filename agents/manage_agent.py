from datetime import datetime
from typing import Any, Dict, Optional

from pydantic import BaseModel, validator

from brokers.test_broker import TestBroker
from data.storage import Storage


class ManagementRequest(BaseModel):
    position_id: str
    action: str  # "adjust", "scale", "close"
    quantity: Optional[float] = None
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
    notes: str = ""

    @validator("quantity")
    def validate_quantity(cls, v, values):
        if v is not None and v <= 0:
            raise ValueError("Quantity must be positive")
        if "action" in values and values["action"] == "scale" and v is None:
            raise ValueError("Scale action requires quantity")
        return v

    @validator("stop_loss", "take_profit")
    def validate_price_levels(cls, v):
        if v is not None and v <= 0:
            raise ValueError("Price levels must be positive")
        return v


class ManageAgent:
    def __init__(self):
        self.broker = TestBroker()
        self.storage = Storage()
        self.max_adjustments = 3
        self.max_scale_quantity = 2.0  # Maximum scale factor
        self.min_profit_threshold = 0.02  # 2% minimum profit for scaling

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Handles position management operations.
        """
        try:
            # Validate and parse the management request
            request = ManagementRequest(**kwargs)

            # Get the position
            position = self.storage.get_position(request.position_id)
            if not position:
                return {
                    "status": "error",
                    "message": f"Position not found: {request.position_id}",
                }

            # Get current market data
            market_data = self.broker.get_market_data([position["symbol"]])
            if not market_data:
                return {
                    "status": "error",
                    "message": f"No market data available for {position['symbol']}",
                }

            # Perform the requested action
            if request.action == "adjust":
                return self._adjust_position(request, position, market_data[position["symbol"]])
            elif request.action == "scale":
                return self._scale_position(request, position, market_data[position["symbol"]])
            elif request.action == "close":
                return self._close_position(request, position, market_data[position["symbol"]])
            else:
                return {
                    "status": "error",
                    "message": f"Unknown action: {request.action}",
                }

        except Exception as e:
            return {
                "status": "error",
                "message": f"Management operation failed: {str(e)}",
            }

    def _adjust_position(
        self,
        request: ManagementRequest,
        position: Dict[str, Any],
        market_data: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Adjust position parameters (stop loss, take profit).
        """
        # Check if we've exceeded maximum adjustments
        adjustment_count = len(
            self.storage.get_history(
                "adjustments",
                start_time=datetime.now().replace(hour=0, minute=0, second=0),
            )
        )
        if adjustment_count >= self.max_adjustments:
            return {"status": "error", "message": "Maximum daily adjustments exceeded"}

        # Update position parameters
        if request.stop_loss is not None:
            position["stop_loss"] = request.stop_loss
        if request.take_profit is not None:
            position["take_profit"] = request.take_profit

        # Save the updated position
        self.storage.save_position(position["position_id"], position)
        self.storage.add_to_history(
            "adjustments",
            {
                "position_id": position["position_id"],
                "stop_loss": request.stop_loss,
                "take_profit": request.take_profit,
            },
        )

        return {
            "status": "success",
            "message": "Position adjusted successfully",
            "data": {"position": position, "market_data": market_data},
        }

    def _scale_position(
        self,
        request: ManagementRequest,
        position: Dict[str, Any],
        market_data: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Scale the position size.
        """
        # Calculate current P&L
        current_price = market_data["price"]
        entry_price = position["entry_price"]
        pnl_pct = (
            (current_price - entry_price) / entry_price
            if position["side"] == "long"
            else (entry_price - current_price) / entry_price
        )

        # Check if we meet the profit threshold for scaling
        if pnl_pct < self.min_profit_threshold:
            return {
                "status": "error",
                "message": f"Insufficient profit ({pnl_pct:.2%}) for scaling",
            }

        # Calculate new quantity
        new_quantity = position["quantity"] * request.quantity
        if new_quantity > position["quantity"] * self.max_scale_quantity:
            return {
                "status": "error",
                "message": f"Scale quantity exceeds maximum limit",
            }

        # Place the scale order
        order_result = self.broker.place_order(
            symbol=position["symbol"],
            order_type="market",
            side=position["side"],
            quantity=new_quantity - position["quantity"],
        )

        # Update position
        position["quantity"] = new_quantity
        self.storage.save_position(position["position_id"], position)
        self.storage.add_to_history(
            "scales",
            {
                "position_id": position["position_id"],
                "old_quantity": position["quantity"],
                "new_quantity": new_quantity,
                "order": order_result,
            },
        )

        return {
            "status": "success",
            "message": "Position scaled successfully",
            "data": {
                "position": position,
                "order": order_result,
                "market_data": market_data,
            },
        }

    def _close_position(
        self,
        request: ManagementRequest,
        position: Dict[str, Any],
        market_data: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Close the position.
        """
        # Place the closing order
        order_result = self.broker.place_order(
            symbol=position["symbol"],
            order_type="market",
            side="sell" if position["side"] == "long" else "buy",
            quantity=position["quantity"],
        )

        # Calculate final P&L
        current_price = market_data["price"]
        entry_price = position["entry_price"]
        pnl = (
            (current_price - entry_price) * position["quantity"]
            if position["side"] == "long"
            else (entry_price - current_price) * position["quantity"]
        )

        # Add to history and remove position
        self.storage.add_to_history(
            "trades",
            {
                "position_id": position["position_id"],
                "symbol": position["symbol"],
                "side": position["side"],
                "quantity": position["quantity"],
                "entry_price": entry_price,
                "exit_price": current_price,
                "pnl": pnl,
                "order": order_result,
            },
        )
        self.storage.save_position(position["position_id"], None)  # Remove position

        return {
            "status": "success",
            "message": "Position closed successfully",
            "data": {
                "position": position,
                "order": order_result,
                "pnl": pnl,
                "market_data": market_data,
            },
        }

    def check_risk_limits(self, position_id: str) -> Dict[str, Any]:
        """
        Check if position is within risk limits.
        """
        position = self.storage.get_position(position_id)
        if not position:
            return {"status": "error", "message": f"Position not found: {position_id}"}

        market_data = self.broker.get_market_data([position["symbol"]])
        if not market_data:
            return {
                "status": "error",
                "message": f"No market data available for {position['symbol']}",
            }

        current_price = market_data[position["symbol"]]["price"]
        entry_price = position["entry_price"]
        pnl = (
            (current_price - entry_price) * position["quantity"]
            if position["side"] == "long"
            else (entry_price - current_price) * position["quantity"]
        )

        # Check stop loss
        if position["stop_loss"] is not None:
            if (position["side"] == "long" and current_price <= position["stop_loss"]) or (
                position["side"] == "short" and current_price >= position["stop_loss"]
            ):
                return {
                    "status": "warning",
                    "message": "Stop loss triggered",
                    "data": {
                        "position": position,
                        "current_price": current_price,
                        "pnl": pnl,
                    },
                }

        # Check take profit
        if position["take_profit"] is not None:
            if (position["side"] == "long" and current_price >= position["take_profit"]) or (
                position["side"] == "short" and current_price <= position["take_profit"]
            ):
                return {
                    "status": "warning",
                    "message": "Take profit triggered",
                    "data": {
                        "position": position,
                        "current_price": current_price,
                        "pnl": pnl,
                    },
                }

        return {
            "status": "success",
            "message": "Position within risk limits",
            "data": {"position": position, "current_price": current_price, "pnl": pnl},
        }
