from typing import Dict, Any, List, Optional
import json
from datetime import datetime
from pathlib import Path
import os

class Storage:
    def __init__(self, base_dir: str = "data/storage"):
        self.base_dir = Path(base_dir)
        self.orders_file = self.base_dir / "orders.json"
        self.positions_file = self.base_dir / "positions.json"
        self.history_file = self.base_dir / "history.json"
        
        # Create storage directory if it doesn't exist
        self.base_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize files if they don't exist
        self._init_files()
    
    def _init_files(self):
        """Initialize storage files with empty data structures."""
        if not self.orders_file.exists():
            self._save_json(self.orders_file, {"orders": {}})
        if not self.positions_file.exists():
            self._save_json(self.positions_file, {"positions": {}})
        if not self.history_file.exists():
            self._save_json(self.history_file, {
                "orders": [],
                "positions": [],
                "trades": []
            })
    
    def _save_json(self, file_path: Path, data: Dict[str, Any]):
        """Save data to a JSON file."""
        with open(file_path, 'w') as f:
            json.dump(data, f, indent=2, default=str)
    
    def _load_json(self, file_path: Path) -> Dict[str, Any]:
        """Load data from a JSON file."""
        with open(file_path, 'r') as f:
            return json.load(f)
    
    def save_order(self, order_id: str, order_data: Dict[str, Any]):
        """Save an order to storage."""
        data = self._load_json(self.orders_file)
        data["orders"][order_id] = order_data
        self._save_json(self.orders_file, data)
    
    def get_order(self, order_id: str) -> Optional[Dict[str, Any]]:
        """Retrieve an order from storage."""
        data = self._load_json(self.orders_file)
        return data["orders"].get(order_id)
    
    def list_orders(self) -> Dict[str, Dict[str, Any]]:
        """List all orders in storage."""
        data = self._load_json(self.orders_file)
        return data["orders"]
    
    def save_position(self, position_id: str, position_data: Dict[str, Any]):
        """Save a position to storage."""
        data = self._load_json(self.positions_file)
        data["positions"][position_id] = position_data
        self._save_json(self.positions_file, data)
    
    def get_position(self, position_id: str) -> Optional[Dict[str, Any]]:
        """Retrieve a position from storage."""
        data = self._load_json(self.positions_file)
        return data["positions"].get(position_id)
    
    def list_positions(self) -> Dict[str, Dict[str, Any]]:
        """List all positions in storage."""
        data = self._load_json(self.positions_file)
        return data["positions"]
    
    def add_to_history(self, item_type: str, item_data: Dict[str, Any]):
        """Add an item to the history."""
        data = self._load_json(self.history_file)
        if item_type not in data:
            data[item_type] = []
        data[item_type].append({
            **item_data,
            "timestamp": datetime.now().isoformat()
        })
        self._save_json(self.history_file, data)
    
    def get_history(self, item_type: str, 
                   start_time: Optional[datetime] = None,
                   end_time: Optional[datetime] = None) -> List[Dict[str, Any]]:
        """Retrieve history items of a specific type within a time range."""
        data = self._load_json(self.history_file)
        items = data.get(item_type, [])
        
        if start_time or end_time:
            filtered_items = []
            for item in items:
                item_time = datetime.fromisoformat(item["timestamp"])
                if start_time and item_time < start_time:
                    continue
                if end_time and item_time > end_time:
                    continue
                filtered_items.append(item)
            return filtered_items
        
        return items 