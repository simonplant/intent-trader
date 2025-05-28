"""Test the basic flow of the trading system."""

import pytest
from src.market_data.feed import MarketDataFeed
from src.strategy.engine import StrategyEngine
from src.order.manager import OrderManager, OrderSide, OrderType
import pandas as pd
import numpy as np

def test_basic_trading_flow():
    """Test the basic flow of market data -> strategy -> order management."""
    
    # Initialize components
    feed = MarketDataFeed()
    engine = StrategyEngine()
    manager = OrderManager()
    
    # Get market data
    data = feed.get_historical_data("AAPL", period="1d", interval="1m")
    assert len(data) > 0, "Should get historical data"
    
    # Convert to DataFrame
    df = pd.DataFrame(data)
    
    # Add strategy
    strategy_config = {
        'short_window': 20,
        'long_window': 50
    }
    engine.add_strategy('sma_crossover', strategy_config)
    
    # Generate signals
    signals = engine.generate_signals(df)
    assert 'sma_crossover' in signals, "Should generate signals"
    
    # Create order based on signal
    if signals['sma_crossover']['signal'] == 1:  # Buy signal
        order = manager.create_order(
            symbol="AAPL",
            side=OrderSide.BUY,
            order_type=OrderType.MARKET,
            quantity=100
        )
        assert order.symbol == "AAPL", "Order should be for AAPL"
        assert order.quantity == 100, "Order quantity should be 100"
        
    # Check position
    position = manager.get_position("AAPL")
    assert isinstance(position, float), "Position should be a float"

if __name__ == "__main__":
    pytest.main([__file__]) 