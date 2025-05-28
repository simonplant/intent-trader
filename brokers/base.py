from typing import Dict, Any, List, Optional
from abc import ABC, abstractmethod
from datetime import datetime

class BrokerInterface(ABC):
    @abstractmethod
    def connect(self) -> bool:
        """Connect to the broker's API."""
        pass
    
    @abstractmethod
    def disconnect(self) -> bool:
        """Disconnect from the broker's API."""
        pass
    
    @abstractmethod
    def get_account_info(self) -> Dict[str, Any]:
        """Get account information."""
        pass
    
    @abstractmethod
    def get_market_data(self, symbols: List[str]) -> Dict[str, Dict[str, Any]]:
        """Get real-time market data for symbols."""
        pass
    
    @abstractmethod
    def place_order(self, 
                   symbol: str,
                   order_type: str,
                   side: str,
                   quantity: float,
                   price: Optional[float] = None,
                   stop_price: Optional[float] = None) -> Dict[str, Any]:
        """Place a new order."""
        pass
    
    @abstractmethod
    def cancel_order(self, order_id: str) -> bool:
        """Cancel an existing order."""
        pass
    
    @abstractmethod
    def modify_order(self,
                    order_id: str,
                    quantity: Optional[float] = None,
                    price: Optional[float] = None,
                    stop_price: Optional[float] = None) -> bool:
        """Modify an existing order."""
        pass
    
    @abstractmethod
    def get_order_status(self, order_id: str) -> Dict[str, Any]:
        """Get the status of an order."""
        pass
    
    @abstractmethod
    def get_positions(self) -> List[Dict[str, Any]]:
        """Get current positions."""
        pass
    
    @abstractmethod
    def get_order_history(self,
                         start_time: Optional[datetime] = None,
                         end_time: Optional[datetime] = None) -> List[Dict[str, Any]]:
        """Get order history."""
        pass
    
    @abstractmethod
    def get_trade_history(self,
                         start_time: Optional[datetime] = None,
                         end_time: Optional[datetime] = None) -> List[Dict[str, Any]]:
        """Get trade history."""
        pass 