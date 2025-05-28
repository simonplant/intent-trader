"""
Order Management System - Core component for handling trading orders.
"""
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional, Union
from uuid import UUID, uuid4
import logging

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
    created_at: datetime = datetime.utcnow()
    updated_at: datetime = datetime.utcnow()
    filled_quantity: float = 0.0
    average_fill_price: Optional[float] = None
    client_order_id: Optional[str] = None
    exchange_order_id: Optional[str] = None

    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'symbol': self.symbol,
            'side': self.side.value,
            'type': self.type.value,
            'quantity': self.quantity,
            'price': self.price,
            'status': self.status.value,
            'created_at': self.created_at.isoformat(),
            'updated_at': self.updated_at.isoformat()
        }

class OrderManager:
    """Manages the creation, modification, and tracking of trading orders."""
    
    def __init__(self):
        """Initialize the OrderManager."""
        self._orders: Dict[UUID, Order] = {}
        self._order_book: Dict[str, List[Order]] = {}
        self.positions: Dict[str, float] = {}
    
    def create_order(
        self,
        symbol: str,
        side: Union[OrderSide, str],
        order_type: Union[OrderType, str],
        quantity: float,
        price: Optional[float] = None,
        stop_price: Optional[float] = None,
        client_order_id: Optional[str] = None
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
            client_order_id=client_order_id
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
        exchange_order_id: Optional[str] = None
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
        order.updated_at = datetime.utcnow()
        
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
        order = self._orders.get(order_id)
        if not order:
            return None
            
        if order.status not in (OrderStatus.PENDING, OrderStatus.SUBMITTED, OrderStatus.PARTIALLY_FILLED):
            raise ValueError(f"Cannot cancel order in {order.status.value} status")
            
        order.status = OrderStatus.CANCELED
        order.updated_at = datetime.utcnow()
        
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
        open_statuses = {OrderStatus.PENDING, OrderStatus.SUBMITTED, OrderStatus.PARTIALLY_FILLED}
        
        if symbol:
            orders = self._order_book.get(symbol, [])
        else:
            orders = list(self._orders.values())
            
        return [order for order in orders if order.status in open_statuses]
    
    def get_position(self, symbol: str) -> float:
        """Get current position size for a symbol."""
        return self.positions.get(symbol, 0.0)

if __name__ == "__main__":
    # Test the order manager
    manager = OrderManager()
    
    # Create a market buy order
    order = manager.create_order(
        symbol="AAPL",
        side=OrderSide.BUY,
        order_type=OrderType.MARKET,
        quantity=100
    )
    print(f"Created order: {order.to_dict()}")
    
    # Update order status
    manager.update_order_status(order.id, OrderStatus.FILLED)
    print(f"Updated order: {order.to_dict()}")
    
    # Get open orders
    open_orders = manager.get_open_orders()
    print(f"Open orders: {len(open_orders)}") 