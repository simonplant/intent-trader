import pytest
from datetime import datetime
from execution.order_manager import OrderManager, OrderParameters
from data.schemas import OrderSchema, PositionSchema, MarketDataSchema

@pytest.fixture
def order_manager():
    """Fixture for order manager"""
    return OrderManager()

@pytest.fixture
def sample_market_data():
    """Fixture for sample market data"""
    return MarketDataSchema(
        symbol="ES",
        price=4500.0,
        change=10.0,
        change_percent=0.22,
        volume=1000000,
        high=4510.0,
        low=4490.0,
        timestamp=datetime.now(),
        additional_data={"atr": 20.0}
    )

@pytest.fixture
def sample_position():
    """Fixture for sample position"""
    return PositionSchema(
        position_id="test_position_1",
        symbol="ES",
        quantity=1.0,
        entry_price=4500.0,
        current_price=4510.0,
        pnl=10.0,
        pnl_percent=0.22,
        timestamp=datetime.now(),
        status="open"
    )

def test_order_parameters_validation():
    """Test order parameters validation"""
    # Test valid parameters
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="market",
        time_in_force="day",
        routing="smart"
    )
    assert params.symbol == "ES"
    
    # Test invalid side
    with pytest.raises(ValueError):
        OrderParameters(
            symbol="ES",
            side="invalid",
            quantity=1.0,
            order_type="market"
        )
        
    # Test invalid order type
    with pytest.raises(ValueError):
        OrderParameters(
            symbol="ES",
            side="buy",
            quantity=1.0,
            order_type="invalid"
        )
        
    # Test invalid time in force
    with pytest.raises(ValueError):
        OrderParameters(
            symbol="ES",
            side="buy",
            quantity=1.0,
            order_type="market",
            time_in_force="invalid"
        )

def test_market_order_execution(order_manager, sample_market_data):
    """Test market order execution"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place market order
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="market"
    )
    
    result = order_manager.place_order(params)
    assert result["status"] == "success"
    assert "execution_price" in result["data"]["metadata"]
    
    # Verify order was stored
    order_id = result["data"]["order_id"]
    assert order_id in order_manager.orders
    assert order_manager.orders[order_id].status == "filled"

def test_limit_order_execution(order_manager, sample_market_data):
    """Test limit order execution"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place limit order above market price (should not execute)
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="limit",
        price=4600.0  # Above current price
    )
    
    result = order_manager.place_order(params)
    assert result["status"] == "success"
    assert order_manager.orders[result["data"]["order_id"]].status == "pending"
    
    # Place limit order below market price (should execute)
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="limit",
        price=4400.0  # Below current price
    )
    
    result = order_manager.place_order(params)
    assert result["status"] == "success"
    assert order_manager.orders[result["data"]["order_id"]].status == "filled"

def test_stop_order_execution(order_manager, sample_market_data):
    """Test stop order execution"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place stop order above market price (should execute)
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="stop",
        stop_price=4600.0  # Above current price
    )
    
    result = order_manager.place_order(params)
    assert result["status"] == "success"
    assert order_manager.orders[result["data"]["order_id"]].status == "filled"
    
    # Place stop order below market price (should not execute)
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="stop",
        stop_price=4400.0  # Below current price
    )
    
    result = order_manager.place_order(params)
    assert result["status"] == "success"
    assert order_manager.orders[result["data"]["order_id"]].status == "pending"

def test_stop_limit_order_execution(order_manager, sample_market_data):
    """Test stop-limit order execution"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place stop-limit order
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="stop_limit",
        price=4600.0,  # Limit price
        stop_price=4550.0  # Stop price
    )
    
    result = order_manager.place_order(params)
    assert result["status"] == "success"
    assert order_manager.orders[result["data"]["order_id"]].status == "pending"

def test_order_cancellation(order_manager, sample_market_data):
    """Test order cancellation"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place limit order
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="limit",
        price=4600.0
    )
    
    result = order_manager.place_order(params)
    order_id = result["data"]["order_id"]
    
    # Cancel order
    result = order_manager.cancel_order(order_id)
    assert result["status"] == "success"
    assert order_manager.orders[order_id].status == "cancelled"
    
    # Try to cancel non-existent order
    result = order_manager.cancel_order("invalid_order_id")
    assert result["status"] == "error"

def test_position_updates(order_manager, sample_market_data):
    """Test position updates after order execution"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place market order to open position
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="market"
    )
    
    result = order_manager.place_order(params)
    assert "ES" in order_manager.positions
    assert order_manager.positions["ES"].quantity == 1.0
    
    # Place market order to close position
    params = OrderParameters(
        symbol="ES",
        side="sell",
        quantity=1.0,
        order_type="market"
    )
    
    result = order_manager.place_order(params)
    assert "ES" not in order_manager.positions  # Position should be closed

def test_get_order_status(order_manager, sample_market_data):
    """Test getting order status"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place market order
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="market"
    )
    
    result = order_manager.place_order(params)
    order_id = result["data"]["order_id"]
    
    # Get order status
    status = order_manager.get_order_status(order_id)
    assert status["status"] == "success"
    assert status["data"]["order_id"] == order_id
    
    # Get status of non-existent order
    status = order_manager.get_order_status("invalid_order_id")
    assert status["status"] == "error"

def test_get_open_orders(order_manager, sample_market_data):
    """Test getting open orders"""
    # Update manager with market data
    order_manager.update_market_data({"ES": sample_market_data})
    
    # Place market order (should be filled)
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="market"
    )
    order_manager.place_order(params)
    
    # Place limit order (should be pending)
    params = OrderParameters(
        symbol="ES",
        side="buy",
        quantity=1.0,
        order_type="limit",
        price=4600.0
    )
    order_manager.place_order(params)
    
    # Get open orders
    result = order_manager.get_open_orders()
    assert result["status"] == "success"
    assert len(result["data"]) == 1  # Only the limit order should be open 