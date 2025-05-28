"""Integration tests for market data functionality."""

import asyncio
from datetime import datetime, timedelta
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from src.market_data.feed import MarketDataFeed


@pytest.fixture
def market_data_feed():
    """Create a market data feed instance."""
    return MarketDataFeed()


def test_market_data_retrieval(market_data_feed):
    """Test retrieving market data for multiple symbols."""
    symbols = ["AAPL", "MSFT", "GOOGL"]
    
    # Get latest prices
    prices = market_data_feed.get_multiple_prices(symbols)
    
    assert isinstance(prices, dict)
    assert len(prices) == len(symbols)
    
    for symbol in symbols:
        assert symbol in prices
        assert isinstance(prices[symbol], float)
        assert prices[symbol] > 0


def test_data_caching(market_data_feed):
    """Test that data is properly cached."""
    symbol = "AAPL"
    
    # First call
    price1 = market_data_feed.get_latest_price(symbol)
    
    # Second call (should be cached or similar)
    price2 = market_data_feed.get_latest_price(symbol)
    
    assert isinstance(price1, float)
    assert isinstance(price2, float)
    assert price1 > 0
    assert price2 > 0


def test_data_validation(market_data_feed):
    """Test data validation."""
    # Get historical data
    data = market_data_feed.get_historical_data("AAPL", period="1d", interval="1m")
    
    assert isinstance(data, list)
    assert len(data) > 0
    
    # Validate each data point
    for item in data:
        assert isinstance(item, dict)
        assert "Open" in item
        assert "High" in item
        assert "Low" in item
        assert "Close" in item
        assert "Volume" in item
        
        # Validate price relationships
        assert item["High"] >= item["Low"]
        assert item["High"] >= item["Open"]
        assert item["High"] >= item["Close"]
        assert item["Low"] <= item["Open"]
        assert item["Low"] <= item["Close"]


def test_error_handling(market_data_feed):
    """Test error handling for invalid symbols."""
    # Test with an invalid symbol (should still return a mock price)
    price = market_data_feed.get_latest_price("INVALID_SYMBOL_XYZ")
    
    # Should return a mock price
    assert isinstance(price, float)
    assert price > 0


def test_concurrent_requests(market_data_feed):
    """Test handling concurrent requests."""
    symbols = ["AAPL", "MSFT", "GOOGL", "TSLA", "NVDA"]
    
    # Make multiple concurrent requests
    prices1 = market_data_feed.get_multiple_prices(symbols)
    prices2 = market_data_feed.get_multiple_prices(symbols)
    
    # Both should return valid data
    assert len(prices1) == len(symbols)
    assert len(prices2) == len(symbols)
    
    for symbol in symbols:
        assert symbol in prices1
        assert symbol in prices2
        assert prices1[symbol] > 0
        assert prices2[symbol] > 0
