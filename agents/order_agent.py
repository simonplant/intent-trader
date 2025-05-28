from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

from pydantic import BaseModel


class OrderType(str, Enum):
    MARKET = "market"
    LIMIT = "limit"
    STOP = "stop"
    STOP_LIMIT = "stop_limit"
    TRAILING_STOP = "trailing_stop"


class OrderSide(str, Enum):
    BUY = "buy"
    SELL = "sell"


class OrderStatus(str, Enum):
    PENDING = "pending"
    FILLED = "filled"
    CANCELLED = "cancelled"
    REJECTED = "rejected"
    PARTIALLY_FILLED = "partially_filled"


class Order(BaseModel):
    order_id: str
    symbol: str
    order_type: OrderType
    side: OrderSide
    quantity: float
    price: Optional[float] = None
    stop_price: Optional[float] = None
    status: OrderStatus
    timestamp: datetime
    filled_quantity: float = 0.0
    average_fill_price: Optional[float] = None
    notes: str = ""


class OrderAgent:
    def __init__(self):
        self.orders: Dict[str, Order] = {}
        self.order_history: List[Order] = []

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Handles order management operations.
        """
        action = kwargs.get("action", "list")

        if action == "create":
            return self._create_order(**kwargs)
        elif action == "cancel":
            return self._cancel_order(**kwargs)
        elif action == "modify":
            return self._modify_order(**kwargs)
        elif action == "list":
            return self._list_orders(**kwargs)
        else:
            return {"status": "error", "message": f"Unknown action: {action}"}

    def _create_order(self, **kwargs) -> Dict[str, Any]:
        """
        Creates a new order.
        """
        # TODO: Implement order creation logic
        order = Order(
            order_id="dummy_id",
            symbol=kwargs.get("symbol", ""),
            order_type=OrderType.MARKET,
            side=OrderSide.BUY,
            quantity=kwargs.get("quantity", 0.0),
            status=OrderStatus.PENDING,
            timestamp=datetime.now(),
        )

        self.orders[order.order_id] = order
        return {
            "status": "success",
            "message": "Order created",
            "data": {"order": order.dict()},
        }

    def _cancel_order(self, **kwargs) -> Dict[str, Any]:
        """
        Cancels an existing order.
        """
        order_id = kwargs.get("order_id")
        if order_id in self.orders:
            order = self.orders[order_id]
            order.status = OrderStatus.CANCELLED
            return {
                "status": "success",
                "message": "Order cancelled",
                "data": {"order": order.dict()},
            }
        return {"status": "error", "message": f"Order not found: {order_id}"}

    def _modify_order(self, **kwargs) -> Dict[str, Any]:
        """
        Modifies an existing order.
        """
        order_id = kwargs.get("order_id")
        if order_id in self.orders:
            order = self.orders[order_id]
            # TODO: Implement order modification logic
            return {
                "status": "success",
                "message": "Order modified",
                "data": {"order": order.dict()},
            }
        return {"status": "error", "message": f"Order not found: {order_id}"}

    def _list_orders(self, **kwargs) -> Dict[str, Any]:
        """
        Lists all orders, optionally filtered by status.
        """
        status = kwargs.get("status")
        orders = [
            order.dict() for order in self.orders.values() if not status or order.status == status
        ]
        return {
            "status": "success",
            "message": "Orders listed",
            "data": {"orders": orders},
        }

    def update_order_status(
        self,
        order_id: str,
        status: OrderStatus,
        filled_quantity: float = 0.0,
        average_fill_price: Optional[float] = None,
    ) -> None:
        """
        Updates the status of an order.
        """
        if order_id in self.orders:
            order = self.orders[order_id]
            order.status = status
            order.filled_quantity = filled_quantity
            order.average_fill_price = average_fill_price

            if status in [
                OrderStatus.FILLED,
                OrderStatus.CANCELLED,
                OrderStatus.REJECTED,
            ]:
                self.order_history.append(order)
                del self.orders[order_id]
