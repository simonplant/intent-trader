"""Database management for the trading system."""

import sqlite3
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
from datetime import datetime

from .config import config
from .logging import log_manager

logger = log_manager.get_logger(__name__)

class DatabaseManager:
    """Manages database operations for the trading system."""
    
    def __init__(self):
        """Initialize the database manager."""
        self.db_path = Path(config.get_database_config().get('path', 'data/db/trading.db'))
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._init_db()
        
    def _init_db(self):
        """Initialize the database with required tables."""
        with self._get_connection() as conn:
            cursor = conn.cursor()
            
            # Create orders table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS orders (
                    id TEXT PRIMARY KEY,
                    symbol TEXT NOT NULL,
                    side TEXT NOT NULL,
                    type TEXT NOT NULL,
                    quantity REAL NOT NULL,
                    price REAL,
                    status TEXT NOT NULL,
                    created_at TIMESTAMP NOT NULL,
                    updated_at TIMESTAMP NOT NULL
                )
            ''')
            
            # Create positions table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS positions (
                    symbol TEXT PRIMARY KEY,
                    quantity REAL NOT NULL,
                    average_price REAL NOT NULL,
                    updated_at TIMESTAMP NOT NULL
                )
            ''')
            
            # Create trades table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS trades (
                    id TEXT PRIMARY KEY,
                    order_id TEXT NOT NULL,
                    symbol TEXT NOT NULL,
                    side TEXT NOT NULL,
                    quantity REAL NOT NULL,
                    price REAL NOT NULL,
                    timestamp TIMESTAMP NOT NULL,
                    FOREIGN KEY (order_id) REFERENCES orders (id)
                )
            ''')
            
            conn.commit()
            
    def _get_connection(self) -> sqlite3.Connection:
        """Get a database connection."""
        conn = sqlite3.connect(str(self.db_path))
        conn.row_factory = sqlite3.Row
        return conn
        
    def save_order(self, order: Dict[str, Any]) -> None:
        """Save an order to the database.
        
        Args:
            order: Dictionary containing order information.
        """
        with self._get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('''
                INSERT OR REPLACE INTO orders (
                    id, symbol, side, type, quantity, price,
                    status, created_at, updated_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                order['id'],
                order['symbol'],
                order['side'],
                order['type'],
                order['quantity'],
                order.get('price'),
                order['status'],
                order['created_at'],
                order['updated_at']
            ))
            conn.commit()
            
    def get_order(self, order_id: str) -> Optional[Dict[str, Any]]:
        """Get an order by ID.
        
        Args:
            order_id: The ID of the order to retrieve.
            
        Returns:
            Dictionary containing order information, or None if not found.
        """
        with self._get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('SELECT * FROM orders WHERE id = ?', (order_id,))
            row = cursor.fetchone()
            return dict(row) if row else None
            
    def update_position(self, symbol: str, quantity: float, price: float) -> None:
        """Update a position in the database.
        
        Args:
            symbol: The trading symbol.
            quantity: The position quantity.
            price: The average price.
        """
        with self._get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('''
                INSERT OR REPLACE INTO positions (
                    symbol, quantity, average_price, updated_at
                ) VALUES (?, ?, ?, ?)
            ''', (symbol, quantity, price, datetime.utcnow()))
            conn.commit()
            
    def get_position(self, symbol: str) -> Optional[Dict[str, Any]]:
        """Get a position by symbol.
        
        Args:
            symbol: The trading symbol.
            
        Returns:
            Dictionary containing position information, or None if not found.
        """
        with self._get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('SELECT * FROM positions WHERE symbol = ?', (symbol,))
            row = cursor.fetchone()
            return dict(row) if row else None
            
    def save_trade(self, trade: Dict[str, Any]) -> None:
        """Save a trade to the database.
        
        Args:
            trade: Dictionary containing trade information.
        """
        with self._get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('''
                INSERT INTO trades (
                    id, order_id, symbol, side, quantity,
                    price, timestamp
                ) VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', (
                trade['id'],
                trade['order_id'],
                trade['symbol'],
                trade['side'],
                trade['quantity'],
                trade['price'],
                trade['timestamp']
            ))
            conn.commit()
            
    def get_trades(self, symbol: Optional[str] = None, 
                  start_time: Optional[datetime] = None,
                  end_time: Optional[datetime] = None) -> List[Dict[str, Any]]:
        """Get trades with optional filtering.
        
        Args:
            symbol: Optional symbol to filter by.
            start_time: Optional start time to filter by.
            end_time: Optional end time to filter by.
            
        Returns:
            List of dictionaries containing trade information.
        """
        query = 'SELECT * FROM trades WHERE 1=1'
        params = []
        
        if symbol:
            query += ' AND symbol = ?'
            params.append(symbol)
            
        if start_time:
            query += ' AND timestamp >= ?'
            params.append(start_time)
            
        if end_time:
            query += ' AND timestamp <= ?'
            params.append(end_time)
            
        query += ' ORDER BY timestamp DESC'
        
        with self._get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute(query, params)
            return [dict(row) for row in cursor.fetchall()]

# Create a global database manager instance
db_manager = DatabaseManager() 