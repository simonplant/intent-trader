"""Tests for the database manager."""

import pytest
import tempfile
from pathlib import Path
from datetime import datetime, timedelta
from src.core.config import ConfigManager, get_config_manager
from src.core.database import DatabaseManager, get_db_manager

@pytest.fixture
def db_manager(tmp_path):
    """Create a temporary database and manager."""
    db_dir = tmp_path / "data" / "db"
    db_dir.mkdir(parents=True)
    
    config_path = tmp_path / "config.yaml"
    config_content = f"""
    database:
        path: {db_dir}/trading.db
    """
    config_path.write_text(config_content)
    
    config = ConfigManager(str(config_path))
    return DatabaseManager()

def test_save_and_get_order(db_manager):
    """Test saving and retrieving orders."""
    order = {
        'id': 'test_order_1',
        'symbol': 'AAPL',
        'side': 'BUY',
        'type': 'MARKET',
        'quantity': 100,
        'price': None,
        'status': 'NEW',
        'created_at': datetime.utcnow(),
        'updated_at': datetime.utcnow()
    }
    
    db_manager.save_order(order)
    retrieved_order = db_manager.get_order('test_order_1')
    
    assert retrieved_order is not None
    assert retrieved_order['id'] == order['id']
    assert retrieved_order['symbol'] == order['symbol']
    assert retrieved_order['quantity'] == order['quantity']

def test_update_position(db_manager):
    """Test updating and retrieving positions."""
    symbol = 'AAPL'
    quantity = 100
    price = 150.0
    
    db_manager.update_position(symbol, quantity, price)
    position = db_manager.get_position(symbol)
    
    assert position is not None
    assert position['symbol'] == symbol
    assert position['quantity'] == quantity
    assert position['average_price'] == price

def test_save_and_get_trades(db_manager):
    """Test saving and retrieving trades."""
    trade = {
        'id': 'test_trade_1',
        'order_id': 'test_order_1',
        'symbol': 'AAPL',
        'side': 'BUY',
        'quantity': 100,
        'price': 150.0,
        'timestamp': datetime.utcnow()
    }
    
    db_manager.save_trade(trade)
    
    # Test getting all trades
    trades = db_manager.get_trades()
    assert len(trades) == 1
    assert trades[0]['id'] == trade['id']
    
    # Test filtering by symbol
    trades = db_manager.get_trades(symbol='AAPL')
    assert len(trades) == 1
    
    trades = db_manager.get_trades(symbol='MSFT')
    assert len(trades) == 0
    
    # Test filtering by time range
    start_time = datetime.utcnow() - timedelta(hours=1)
    end_time = datetime.utcnow() + timedelta(hours=1)
    
    trades = db_manager.get_trades(start_time=start_time, end_time=end_time)
    assert len(trades) == 1
    
    trades = db_manager.get_trades(start_time=end_time)
    assert len(trades) == 0

def test_database_initialization(db_manager):
    """Test that database tables are created correctly."""
    with db_manager._get_connection() as conn:
        cursor = conn.cursor()
        
        # Check orders table
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='orders'")
        assert cursor.fetchone() is not None
        
        # Check positions table
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='positions'")
        assert cursor.fetchone() is not None
        
        # Check trades table
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='trades'")
        assert cursor.fetchone() is not None

def test_nonexistent_order(db_manager):
    """Test retrieving a nonexistent order."""
    order = db_manager.get_order('nonexistent_order')
    assert order is None

def test_nonexistent_position(db_manager):
    """Test retrieving a nonexistent position."""
    position = db_manager.get_position('nonexistent_symbol')
    assert position is None 