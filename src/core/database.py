"""Database manager for the trading system."""

import sqlite3
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from .logging import get_log_manager


class DatabaseManager:
    """Manages database operations for the trading system."""

    def __init__(self, config=None):
        """Initialize the database manager.

        Args:
            config: Configuration manager instance.
        """
        self.config = config
        self.logger = get_log_manager(config).get_logger(__name__)

        # Default database path if no config provided
        db_path = "data/db/trading.db"
        if config:
            db_path = config.get("database.path", db_path)

        self.db_path = Path(db_path)
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._init_db()

    def _init_db(self):
        """Initialize the database with required tables."""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()

            # Create orders table
            cursor.execute(
                """
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
            """
            )

            # Create positions table
            cursor.execute(
                """
                CREATE TABLE IF NOT EXISTS positions (
                    symbol TEXT PRIMARY KEY,
                    quantity REAL NOT NULL,
                    average_price REAL NOT NULL,
                    created_at TIMESTAMP NOT NULL,
                    updated_at TIMESTAMP NOT NULL
                )
            """
            )

            # Create trades table
            cursor.execute(
                """
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
            """
            )

            conn.commit()

    def execute_query(self, query: str, params: tuple = ()) -> List[Dict[str, Any]]:
        """Execute a database query.

        Args:
            query: SQL query to execute.
            params: Query parameters.

        Returns:
            List of dictionaries containing query results.
        """
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.row_factory = sqlite3.Row
                cursor = conn.cursor()
                cursor.execute(query, params)
                return [dict(row) for row in cursor.fetchall()]
        except Exception as e:
            self.logger.error(f"Database error: {str(e)}")
            raise

    def insert_order(self, order_data: Dict[str, Any]):
        """Insert a new order into the database.

        Args:
            order_data: Dictionary containing order data.
        """
        query = """
            INSERT INTO orders (
                id, symbol, side, type, quantity, price,
                status, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """
        params = (
            order_data["id"],
            order_data["symbol"],
            order_data["side"],
            order_data["type"],
            order_data["quantity"],
            order_data.get("price"),
            order_data["status"],
            datetime.now().isoformat(),
            datetime.now().isoformat(),
        )
        self.execute_query(query, params)

    def update_order_status(self, order_id: str, status: str):
        """Update the status of an order.

        Args:
            order_id: Order ID.
            status: New status.
        """
        query = """
            UPDATE orders
            SET status = ?, updated_at = ?
            WHERE id = ?
        """
        params = (status, datetime.now().isoformat(), order_id)
        self.execute_query(query, params)

    def get_order(self, order_id: str) -> Optional[Dict[str, Any]]:
        """Get an order by ID.

        Args:
            order_id: Order ID.

        Returns:
            Order data if found, None otherwise.
        """
        query = "SELECT * FROM orders WHERE id = ?"
        results = self.execute_query(query, (order_id,))
        return results[0] if results else None

    def get_open_orders(self) -> List[Dict[str, Any]]:
        """Get all open orders.

        Returns:
            List of open orders.
        """
        query = 'SELECT * FROM orders WHERE status = "open"'
        return self.execute_query(query)

    def get_position(self, symbol: str) -> Optional[Dict[str, Any]]:
        """Get a position by symbol.

        Args:
            symbol: Symbol.

        Returns:
            Position data if found, None otherwise.
        """
        query = "SELECT * FROM positions WHERE symbol = ?"
        results = self.execute_query(query, (symbol,))
        return results[0] if results else None

    def update_position(self, position_data: Dict[str, Any]):
        """Update a position.

        Args:
            position_data: Dictionary containing position data.
        """
        query = """
            INSERT INTO positions (
                symbol, quantity, average_price, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?)
            ON CONFLICT(symbol) DO UPDATE SET
                quantity = ?,
                average_price = ?,
                updated_at = ?
        """
        now = datetime.now().isoformat()
        params = (
            position_data["symbol"],
            position_data["quantity"],
            position_data["average_price"],
            now,
            now,
            position_data["quantity"],
            position_data["average_price"],
            now,
        )
        self.execute_query(query, params)

    def save_trade(self, trade_data: Dict[str, Any]):
        """Save a trade to the database, ensuring unique trade IDs."""
        # Check if trade ID already exists
        existing_trade = self.execute_query(
            "SELECT id FROM trades WHERE id = ?", (trade_data["id"],)
        )
        if existing_trade:
            raise ValueError(f"Trade ID {trade_data['id']} already exists.")

        query = """
            INSERT INTO trades (
                id, order_id, symbol, side, quantity, price, timestamp
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
        """
        params = (
            trade_data["id"],
            trade_data["order_id"],
            trade_data["symbol"],
            trade_data["side"],
            trade_data["quantity"],
            trade_data["price"],
            trade_data["timestamp"],
        )
        self.execute_query(query, params)

    def get_trades(self) -> List[Dict[str, Any]]:
        """Retrieve all trades from the database.

        Returns:
            List of trades.
        """
        query = "SELECT * FROM trades"
        return self.execute_query(query)

    def _get_connection(self):
        """Get a database connection.

        Returns:
            A SQLite connection object.
        """
        return sqlite3.connect(self.db_path)


# Global database manager instance will be created when needed
_db_manager: Optional[DatabaseManager] = None


def get_db_manager(config=None) -> DatabaseManager:
    """Get or create the global database manager instance.

    Args:
        config: Configuration manager instance.

    Returns:
        DatabaseManager instance.
    """
    global _db_manager
    if _db_manager is None:
        _db_manager = DatabaseManager(config)
    return _db_manager
