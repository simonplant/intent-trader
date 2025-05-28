from typing import Dict, Any, List, Optional
from datetime import datetime
import uuid
from .base import BrokerInterface

class TestBroker(BrokerInterface):
    def __init__(self):
        self.connected = False
        self.orders: Dict[str, Dict[str, Any]] = {}
        self.positions: List[Dict[str, Any]] = []
        self.market_data: Dict[str, Dict[str, Any]] = {}
        self.account_info = {
            "account_id": "TEST-123",
            "balance": 100000.0,
            "equity": 100000.0,
            "margin": 0.0,
            "free_margin": 100000.0
        }
    
    def connect(self) -> bool:
        self.connected = True
        return True
    
    def disconnect(self) -> bool:
        self.connected = False
        return True
    
    def get_account_info(self) -> Dict[str, Any]:
        return self.account_info
    
    def get_market_data(self, symbols: List[str]) -> Dict[str, Dict[str, Any]]:
        data = {}
        for symbol in symbols:
            if symbol not in self.market_data:
                # Generate random price data for testing
                base_price = 100.0
                if symbol.startswith("ES"):
                    base_price = 4000.0
                elif symbol.startswith("BTC"):
                    base_price = 50000.0
                
                self.market_data[symbol] = {
                    "price": base_price,
                    "bid": base_price - 0.1,
                    "ask": base_price + 0.1,
                    "volume": 1000,
                    "timestamp": datetime.now().isoformat()
                }
            data[symbol] = self.market_data[symbol]
        return data
    
    def place_order(self, 
                   symbol: str,
                   order_type: str,
                   side: str,
                   quantity: float,
                   price: Optional[float] = None,
                   stop_price: Optional[float] = None) -> Dict[str, Any]:
        order_id = str(uuid.uuid4())
        order = {
            "order_id": order_id,
            "symbol": symbol,
            "order_type": order_type,
            "side": side,
            "quantity": quantity,
            "price": price,
            "stop_price": stop_price,
            "status": "pending",
            "timestamp": datetime.now().isoformat()
        }
        self.orders[order_id] = order
        return order
    
    def cancel_order(self, order_id: str) -> bool:
        if order_id in self.orders:
            self.orders[order_id]["status"] = "cancelled"
            return True
        return False
    
    def modify_order(self,
                    order_id: str,
                    quantity: Optional[float] = None,
                    price: Optional[float] = None,
                    stop_price: Optional[float] = None) -> bool:
        if order_id in self.orders:
            order = self.orders[order_id]
            if quantity is not None:
                order["quantity"] = quantity
            if price is not None:
                order["price"] = price
            if stop_price is not None:
                order["stop_price"] = stop_price
            return True
        return False
    
    def get_order_status(self, order_id: str) -> Dict[str, Any]:
        return self.orders.get(order_id, {})
    
    def get_positions(self) -> List[Dict[str, Any]]:
        return self.positions
    
    def get_order_history(self,
                         start_time: Optional[datetime] = None,
                         end_time: Optional[datetime] = None) -> List[Dict[str, Any]]:
        return list(self.orders.values())
    
    def get_trade_history(self,
                         start_time: Optional[datetime] = None,
                         end_time: Optional[datetime] = None) -> List[Dict[str, Any]]:
        return []  # No trade history in test implementation
    
    def simulate_order_fill(self, order_id: str, fill_price: Optional[float] = None) -> bool:
        """Simulate an order fill for testing."""
        if order_id in self.orders:
            order = self.orders[order_id]
            order["status"] = "filled"
            if fill_price is not None:
                order["fill_price"] = fill_price
            else:
                order["fill_price"] = order.get("price", 0.0)
            
            # Update position
            position = {
                "symbol": order["symbol"],
                "side": order["side"],
                "quantity": order["quantity"],
                "entry_price": order["fill_price"],
                "current_price": order["fill_price"],
                "unrealized_pnl": 0.0,
                "timestamp": datetime.now().isoformat()
            }
            self.positions.append(position)
            return True
        return False 