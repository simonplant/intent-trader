"""Unit tests for the OrderManager class."""

from uuid import UUID

import pytest

from src.order.manager import Order, OrderManager, OrderSide, OrderStatus, OrderType


@pytest.fixture
def order_manager():
    """Create an OrderManager instance for testing."""
    return OrderManager()


@pytest.fixture
def sample_order(order_manager):
    """Create a sample order for testing."""
    return order_manager.create_order(
        symbol="BTC/USD",
        side=OrderSide.BUY,
        order_type=OrderType.LIMIT,
        quantity=1.0,
        price=50000.0,
    )


def test_create_market_order(order_manager):
    """Test creating a market order."""
    order = order_manager.create_order(
        symbol="BTC/USD", side="buy", order_type="market", quantity=1.0
    )

    assert isinstance(order, Order)
    assert order.symbol == "BTC/USD"
    assert order.side == OrderSide.BUY
    assert order.type == OrderType.MARKET
    assert order.quantity == 1.0
    assert order.price is None
    assert order.status == OrderStatus.PENDING


def test_create_limit_order(order_manager):
    """Test creating a limit order."""
    order = order_manager.create_order(
        symbol="ETH/USD",
        side=OrderSide.SELL,
        order_type=OrderType.LIMIT,
        quantity=10.0,
        price=3000.0,
    )

    assert isinstance(order, Order)
    assert order.symbol == "ETH/USD"
    assert order.side == OrderSide.SELL
    assert order.type == OrderType.LIMIT
    assert order.quantity == 10.0
    assert order.price == 3000.0
    assert order.status == OrderStatus.PENDING


def test_create_stop_order(order_manager):
    """Test creating a stop order."""
    order = order_manager.create_order(
        symbol="BTC/USD",
        side=OrderSide.SELL,
        order_type=OrderType.STOP,
        quantity=1.0,
        stop_price=45000.0,
    )

    assert isinstance(order, Order)
    assert order.symbol == "BTC/USD"
    assert order.side == OrderSide.SELL
    assert order.type == OrderType.STOP
    assert order.quantity == 1.0
    assert order.stop_price == 45000.0
    assert order.status == OrderStatus.PENDING


def test_create_invalid_limit_order(order_manager):
    """Test creating a limit order without price."""
    with pytest.raises(ValueError):
        order_manager.create_order(
            symbol="BTC/USD",
            side=OrderSide.BUY,
            order_type=OrderType.LIMIT,
            quantity=1.0,
        )


def test_create_invalid_stop_order(order_manager):
    """Test creating a stop order without stop price."""
    with pytest.raises(ValueError):
        order_manager.create_order(
            symbol="BTC/USD",
            side=OrderSide.SELL,
            order_type=OrderType.STOP,
            quantity=1.0,
        )


def test_get_order(order_manager, sample_order):
    """Test retrieving an order by ID."""
    retrieved_order = order_manager.get_order(sample_order.id)
    assert retrieved_order == sample_order


def test_get_nonexistent_order(order_manager):
    """Test retrieving a nonexistent order."""
    retrieved_order = order_manager.get_order(UUID("00000000-0000-0000-0000-000000000000"))
    assert retrieved_order is None


def test_get_orders_by_symbol(order_manager):
    """Test retrieving orders by symbol."""
    # Create multiple orders for the same symbol
    order1 = order_manager.create_order(
        symbol="BTC/USD", side=OrderSide.BUY, order_type=OrderType.MARKET, quantity=1.0
    )
    order2 = order_manager.create_order(
        symbol="BTC/USD",
        side=OrderSide.SELL,
        order_type=OrderType.LIMIT,
        quantity=2.0,
        price=50000.0,
    )

    # Create an order for a different symbol
    order_manager.create_order(
        symbol="ETH/USD", side=OrderSide.BUY, order_type=OrderType.MARKET, quantity=10.0
    )

    btc_orders = order_manager.get_orders_by_symbol("BTC/USD")
    assert len(btc_orders) == 2
    assert order1 in btc_orders
    assert order2 in btc_orders


def test_update_order_status(order_manager, sample_order):
    """Test updating order status."""
    updated_order = order_manager.update_order_status(
        order_id=sample_order.id,
        status=OrderStatus.FILLED,
        filled_quantity=1.0,
        average_fill_price=50000.0,
        exchange_order_id="EXCHANGE-123",
    )

    assert updated_order.status == OrderStatus.FILLED
    assert updated_order.filled_quantity == 1.0
    assert updated_order.average_fill_price == 50000.0
    assert updated_order.exchange_order_id == "EXCHANGE-123"
    assert updated_order.updated_at > sample_order.created_at


def test_update_nonexistent_order(order_manager):
    """Test updating a nonexistent order."""
    updated_order = order_manager.update_order_status(
        order_id=UUID("00000000-0000-0000-0000-000000000000"), status=OrderStatus.FILLED
    )
    assert updated_order is None


def test_cancel_order(order_manager, sample_order):
    """Test canceling an order."""
    canceled_order = order_manager.cancel_order(sample_order.id)
    assert canceled_order.status == OrderStatus.CANCELED
    assert canceled_order.updated_at > sample_order.created_at


def test_cancel_filled_order(order_manager):
    """Test canceling a filled order."""
    order = order_manager.create_order(
        symbol="BTC/USD", side=OrderSide.BUY, order_type=OrderType.MARKET, quantity=1.0
    )
    order_manager.update_order_status(
        order_id=order.id, status=OrderStatus.FILLED, filled_quantity=1.0
    )

    with pytest.raises(ValueError):
        order_manager.cancel_order(order.id)


def test_get_open_orders(order_manager):
    """Test retrieving open orders."""
    # Create orders with different statuses
    pending_order = order_manager.create_order(
        symbol="BTC/USD", side=OrderSide.BUY, order_type=OrderType.MARKET, quantity=1.0
    )

    filled_order = order_manager.create_order(
        symbol="BTC/USD", side=OrderSide.SELL, order_type=OrderType.MARKET, quantity=1.0
    )
    order_manager.update_order_status(
        order_id=filled_order.id, status=OrderStatus.FILLED, filled_quantity=1.0
    )

    canceled_order = order_manager.create_order(
        symbol="BTC/USD", side=OrderSide.BUY, order_type=OrderType.MARKET, quantity=1.0
    )
    order_manager.cancel_order(canceled_order.id)

    # Test getting all open orders
    open_orders = order_manager.get_open_orders()
    assert len(open_orders) == 1
    assert pending_order in open_orders

    # Test getting open orders for specific symbol
    open_btc_orders = order_manager.get_open_orders("BTC/USD")
    assert len(open_btc_orders) == 1
    assert pending_order in open_btc_orders
