"""Tests for database functionality."""

import tempfile
from datetime import datetime
from pathlib import Path

import pytest

from src.core.config import ConfigManager
from src.core.database import DatabaseManager


@pytest.fixture
def db_manager(tmp_path):
    """Create a temporary database for testing."""
    db_path = tmp_path / "test.db"

    config_path = tmp_path / "config.yaml"
    config_content = f"""
    database:
        path: {db_path}
    """
    config_path.write_text(config_content)

    config = ConfigManager(str(config_path))
    return DatabaseManager(config)


def test_save_and_get_order(db_manager):
    """Test saving and retrieving an order."""
    order_data = {
        "id": "test_order_1",
        "symbol": "AAPL",
        "side": "buy",
        "type": "limit",
        "quantity": 100,
        "price": 150.0,
        "status": "open",
        "created_at": datetime.now(),
        "updated_at": datetime.now(),
    }

    # Save order
    db_manager.insert_order(order_data)

    # Retrieve order
    retrieved_order = db_manager.get_order("test_order_1")

    assert retrieved_order is not None
    assert retrieved_order["id"] == "test_order_1"
    assert retrieved_order["symbol"] == "AAPL"
    assert retrieved_order["side"] == "buy"
    assert retrieved_order["quantity"] == 100
    assert retrieved_order["price"] == 150.0
    assert retrieved_order["status"] == "open"


def test_update_order_status(db_manager):
    """Test updating order status."""
    order_data = {
        "id": "test_order_2",
        "symbol": "MSFT",
        "side": "sell",
        "type": "market",
        "quantity": 50,
        "status": "open",
    }

    # Save order
    db_manager.insert_order(order_data)

    # Update status
    db_manager.update_order_status("test_order_2", "filled")

    # Verify update
    updated_order = db_manager.get_order("test_order_2")
    assert updated_order["status"] == "filled"


def test_get_open_orders(db_manager):
    """Test retrieving open orders."""
    # Save multiple orders
    orders = [
        {
            "id": "test_order_3",
            "symbol": "GOOGL",
            "side": "buy",
            "type": "limit",
            "quantity": 10,
            "price": 2500.0,
            "status": "open",
        },
        {
            "id": "test_order_4",
            "symbol": "TSLA",
            "side": "sell",
            "type": "limit",
            "quantity": 20,
            "price": 800.0,
            "status": "filled",
        },
        {
            "id": "test_order_5",
            "symbol": "NVDA",
            "side": "buy",
            "type": "limit",
            "quantity": 15,
            "price": 600.0,
            "status": "open",
        },
    ]

    for order in orders:
        db_manager.insert_order(order)

    # Get open orders
    open_orders = db_manager.get_open_orders()

    assert len(open_orders) == 2
    assert all(order["status"] == "open" for order in open_orders)


def test_update_position(db_manager):
    """Test updating positions."""
    position_data = {
        "symbol": "AAPL",
        "quantity": 100,
        "average_price": 150.0,
    }

    # Update position
    db_manager.update_position(position_data)

    # Retrieve position
    position = db_manager.get_position("AAPL")

    assert position is not None
    assert position["symbol"] == "AAPL"
    assert position["quantity"] == 100
    assert position["average_price"] == 150.0

    # Update position again
    position_data["quantity"] = 200
    position_data["average_price"] = 155.0
    db_manager.update_position(position_data)

    # Verify update
    updated_position = db_manager.get_position("AAPL")
    assert updated_position["quantity"] == 200
    assert updated_position["average_price"] == 155.0


def test_save_and_get_trades(db_manager):
    """Test saving and retrieving trades."""
    # First, insert an order that the trade references
    order_data = {
        "id": "test_order_6",
        "symbol": "AAPL",
        "side": "buy",
        "type": "market",
        "quantity": 100,
        "status": "filled",
    }
    db_manager.insert_order(order_data)

    # Save trade
    trade_data = {
        "id": "test_trade_1",
        "order_id": "test_order_6",
        "symbol": "AAPL",
        "side": "buy",
        "quantity": 100,
        "price": 150.0,
        "timestamp": datetime.now().isoformat(),
    }

    db_manager.save_trade(trade_data)

    # Retrieve trades
    trades = db_manager.get_trades()

    assert len(trades) == 1
    assert trades[0]["id"] == "test_trade_1"
    assert trades[0]["symbol"] == "AAPL"
    assert trades[0]["quantity"] == 100
    assert trades[0]["price"] == 150.0
