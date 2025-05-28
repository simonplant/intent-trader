"""Test the basic trading flow."""

from datetime import datetime, timedelta
from unittest.mock import Mock, patch

import numpy as np
import pandas as pd
import pytest

from src.market_data.feed import MarketDataFeed
from src.order.manager import OrderManager, OrderSide, OrderType
from src.strategy.engine import StrategyEngine


def test_basic_trading_flow():
    """Test the basic flow from market data to order creation."""
    # Initialize components
    market_data = MarketDataFeed()
    strategy_engine = StrategyEngine()
    order_manager = OrderManager()

    # Get historical data
    end_date = datetime.now()
    start_date = end_date - timedelta(days=30)
    data = market_data.get_historical_data(
        symbols="AAPL", start_date=start_date, end_date=end_date, interval="1d"
    )

    assert not data.empty, "Should receive market data"
    assert isinstance(data, pd.DataFrame), "Data should be a DataFrame"

    # Add technical indicators
    data = market_data.add_technical_indicators(data)

    # Generate trading signals
    data = strategy_engine.generate_signals(
        data=data,
        strategy="sma_crossover",
        params={"short_window": 20, "long_window": 50},
    )

    assert "signal" in data.columns, "Should have signal column"
    assert data["signal"].isin([-1, 0, 1]).all(), "Signals should be -1, 0, or 1"

    # Create order if we have a buy signal
    latest_signal = data["signal"].iloc[-1]
    if latest_signal == 1:
        order = order_manager.create_order(
            symbol="AAPL", side=OrderSide.BUY, type=OrderType.MARKET, quantity=100
        )

        assert order is not None, "Should create order"
        assert order.symbol == "AAPL", "Order should be for AAPL"
        assert order.quantity == 100, "Order should be for 100 shares"

    # Check position
    position = order_manager.get_position("AAPL")
    assert isinstance(position, float), "Position should be a float"


if __name__ == "__main__":
    pytest.main([__file__])
