from datetime import datetime
from typing import Any, Dict, Optional

from pydantic import BaseModel, validator

from brokers.test_broker import TestBroker
from data.storage import Storage


class ExecutionRequest(BaseModel):
    symbol: str
    side: str
    quantity: float
    order_type: str
    price: Optional[float] = None
    stop_price: Optional[float] = None
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
    notes: str = ""

    @validator("quantity")
    def validate_quantity(cls, v):
        if v <= 0:
            raise ValueError("Quantity must be positive")
        return v

    @validator("price")
    def validate_price(cls, v, values):
        if v is not None and v <= 0:
            raise ValueError("Price must be positive")
        if "order_type" in values and values["order_type"] == "limit" and v is None:
            raise ValueError("Limit orders require a price")
        return v

    @validator("stop_price")
    def validate_stop_price(cls, v, values):
        if v is not None and v <= 0:
            raise ValueError("Stop price must be positive")
        if "order_type" in values and values["order_type"] in ["stop", "stop_limit"] and v is None:
            raise ValueError("Stop orders require a stop price")
        return v


class ExecuteAgent:
    def __init__(self):
        self.broker = TestBroker()
        self.storage = Storage()
        self.max_position_size = 1000000  # $1M max position
        self.max_daily_trades = 10
        self.daily_trades = 0
        self.last_trade_date = None

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Handles trade execution with safety checks and validation.
        """
        try:
            # Validate and parse the execution request
            request = ExecutionRequest(**kwargs)

            # Perform safety checks
            safety_check = self._perform_safety_checks(request)
            if not safety_check["status"] == "success":
                return safety_check

            # Get current market data
            market_data = self.broker.get_market_data([request.symbol])
            if not market_data:
                return {
                    "status": "error",
                    "message": f"No market data available for {request.symbol}",
                }

            # Calculate position size and check limits
            position_value = self._calculate_position_value(request, market_data[request.symbol])
            if position_value > self.max_position_size:
                return {
                    "status": "error",
                    "message": f"Position size {position_value} exceeds maximum limit of {self.max_position_size}",
                }

            # Place the order
            order_result = self.broker.place_order(
                symbol=request.symbol,
                order_type=request.order_type,
                side=request.side,
                quantity=request.quantity,
                price=request.price,
                stop_price=request.stop_price,
            )

            # Store the order
            self.storage.save_order(order_result["order_id"], order_result)
            self.storage.add_to_history("orders", order_result)

            # Update trade count
            self._update_trade_count()

            return {
                "status": "success",
                "message": "Order executed successfully",
                "data": {
                    "order": order_result,
                    "market_data": market_data[request.symbol],
                },
            }

        except Exception as e:
            return {"status": "error", "message": f"Execution failed: {str(e)}"}

    def _perform_safety_checks(self, request: ExecutionRequest) -> Dict[str, Any]:
        """
        Perform safety checks before execution.
        """
        # Check if we've exceeded daily trade limit
        if self._check_daily_trade_limit():
            return {"status": "error", "message": "Daily trade limit exceeded"}

        # Check if we have existing positions
        existing_positions = self.storage.list_positions()
        if request.symbol in [p["symbol"] for p in existing_positions.values()]:
            return {
                "status": "error",
                "message": f"Position already exists for {request.symbol}",
            }

        # Check if we have sufficient buying power
        account_info = self.broker.get_account_info()
        if request.side == "buy" and account_info["free_margin"] < self.max_position_size:
            return {"status": "error", "message": "Insufficient buying power"}

        return {"status": "success"}

    def _calculate_position_value(
        self, request: ExecutionRequest, market_data: Dict[str, Any]
    ) -> float:
        """
        Calculate the total value of the position.
        """
        price = request.price or market_data["price"]
        return price * request.quantity

    def _check_daily_trade_limit(self) -> bool:
        """
        Check if we've exceeded the daily trade limit.
        """
        today = datetime.now().date()
        if self.last_trade_date != today:
            self.daily_trades = 0
            self.last_trade_date = today
        return self.daily_trades >= self.max_daily_trades

    def _update_trade_count(self):
        """
        Update the daily trade count.
        """
        today = datetime.now().date()
        if self.last_trade_date != today:
            self.daily_trades = 0
            self.last_trade_date = today
        self.daily_trades += 1

    def get_execution_status(self, order_id: str) -> Dict[str, Any]:
        """
        Get the status of an executed order.
        """
        order = self.storage.get_order(order_id)
        if not order:
            return {"status": "error", "message": f"Order not found: {order_id}"}

        broker_status = self.broker.get_order_status(order_id)
        return {
            "status": "success",
            "data": {"order": order, "broker_status": broker_status},
        }
