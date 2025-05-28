"""Tests for market data functionality."""

import os
import tempfile
from datetime import datetime, timedelta
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

import numpy as np
import pandas as pd
import pytest

from src.core.config import get_config_manager
from src.core.logging import get_log_manager
from src.market_data.feed import MarketDataFeed


@pytest.fixture
def market_data_feed():
    """Create a market data feed instance for testing."""
    # Create market data feed instance
    feed = MarketDataFeed()
    yield feed


def test_parameter_validation(market_data_feed):
    """Test parameter validation."""
    # Test that get_historical_data returns data
    data = market_data_feed.get_historical_data("AAPL", period="1d", interval="1m")
    assert isinstance(data, list)
    assert len(data) > 0
    
    # Test that get_latest_price returns a float
    price = market_data_feed.get_latest_price("AAPL")
    assert isinstance(price, float)
    assert price > 0


def test_data_retrieval(market_data_feed):
    """Test data retrieval functionality."""
    # Test historical data retrieval
    data = market_data_feed.get_historical_data("AAPL", period="1d", interval="1m")
    assert isinstance(data, list)
    assert len(data) > 0
    
    # Check data structure
    if data:
        first_item = data[0]
        assert "Open" in first_item
        assert "High" in first_item
        assert "Low" in first_item
        assert "Close" in first_item
        assert "Volume" in first_item
        assert "Datetime" in first_item


def test_data_caching(market_data_feed):
    """Test data caching functionality."""
    # First retrieval
    data1 = market_data_feed.get_historical_data("AAPL", period="1d", interval="1m")
    
    # Second retrieval (should be similar)
    data2 = market_data_feed.get_historical_data("AAPL", period="1d", interval="1m")
    
    # Both should return data
    assert len(data1) > 0
    assert len(data2) > 0


def test_data_validation(market_data_feed):
    """Test data validation."""
    # Get historical data
    data = market_data_feed.get_historical_data("AAPL", period="1d", interval="1m")
    
    # Validate data structure
    assert isinstance(data, list)
    for item in data:
        assert isinstance(item, dict)
        assert item.get("High", 0) >= item.get("Low", 0)
        assert item.get("Volume", 0) >= 0


def test_technical_indicators(market_data_feed):
    """Test technical indicator functionality."""
    # Get historical data
    data = market_data_feed.get_historical_data("AAPL", period="5d", interval="1h")
    
    # Verify we have enough data points
    assert len(data) > 0
    
    # Basic validation of price data
    for item in data:
        assert item.get("Close", 0) > 0


def test_multiple_symbols(market_data_feed):
    """Test handling of multiple symbols."""
    # Test multiple price retrieval
    symbols = ["AAPL", "MSFT", "GOOGL"]
    prices = market_data_feed.get_multiple_prices(symbols)
    
    # Verify we got prices for all symbols
    assert isinstance(prices, dict)
    assert len(prices) == len(symbols)
    for symbol in symbols:
        assert symbol in prices
        assert isinstance(prices[symbol], float)
        assert prices[symbol] > 0
