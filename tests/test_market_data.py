"""Tests for the market data system."""

import pytest
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
import os

from src.market.data import MarketDataFeed
from src.core.config import config
from src.core.logging import log_manager

@pytest.fixture
def market_data_feed():
    """Create a market data feed instance for testing."""
    # Create test cache directory
    cache_dir = Path("data/cache/test")
    cache_dir.mkdir(parents=True, exist_ok=True)
    
    # Create market data feed instance
    feed = MarketDataFeed(config, log_manager.get_logger(__name__), str(cache_dir))
    
    yield feed
    
    # Cleanup
    for file in cache_dir.glob("*"):
        file.unlink()
    cache_dir.rmdir()

def test_parameter_validation(market_data_feed):
    """Test parameter validation."""
    # Test invalid symbols
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data(123, datetime.now(), datetime.now())
    
    # Test invalid dates
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data("AAPL", "2023-01-01", datetime.now())
    
    # Test invalid interval
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data("AAPL", datetime.now(), datetime.now(), interval="2d")
    
    # Test invalid source
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data("AAPL", datetime.now(), datetime.now(), source="invalid")

def test_data_retrieval(market_data_feed):
    """Test data retrieval from different sources."""
    end_date = datetime.now()
    start_date = end_date - timedelta(days=7)
    
    # Test Alpaca data retrieval
    alpaca_data = market_data_feed.get_historical_data(
        "AAPL",
        start_date,
        end_date,
        source="alpaca"
    )
    assert isinstance(alpaca_data, pd.DataFrame)
    assert not alpaca_data.empty
    assert all(col in alpaca_data.columns for col in ['symbol', 'timestamp', 'open', 'high', 'low', 'close', 'volume'])
    
    # Test Yahoo Finance data retrieval
    yf_data = market_data_feed.get_historical_data(
        "AAPL",
        start_date,
        end_date,
        source="yfinance"
    )
    assert isinstance(yf_data, pd.DataFrame)
    assert not yf_data.empty
    assert all(col in yf_data.columns for col in ['symbol', 'timestamp', 'open', 'high', 'low', 'close', 'volume'])

def test_data_caching(market_data_feed):
    """Test data caching functionality."""
    end_date = datetime.now()
    start_date = end_date - timedelta(days=7)
    
    # First retrieval should fetch from API
    data1 = market_data_feed.get_historical_data("AAPL", start_date, end_date)
    
    # Second retrieval should use cache
    data2 = market_data_feed.get_historical_data("AAPL", start_date, end_date)
    
    # Verify data consistency
    pd.testing.assert_frame_equal(data1, data2)
    
    # Verify cache file exists
    cache_file = market_data_feed.cache_dir / f"AAPL_1d_{start_date.date()}_{end_date.date()}.parquet"
    assert cache_file.exists()

def test_data_validation(market_data_feed):
    """Test data validation and cleaning."""
    # Create test data with invalid relationships
    data = pd.DataFrame({
        'symbol': ['AAPL'] * 5,
        'timestamp': pd.date_range(start='2023-01-01', periods=5),
        'open': [100, 101, 102, 103, 104],
        'high': [99, 101, 102, 103, 104],  # Invalid: high < open
        'low': [105, 101, 102, 103, 104],  # Invalid: low > open
        'close': [101, 102, 103, 104, 105],
        'volume': [1000, 2000, 3000, 4000, 5000]
    })
    
    # Validate data
    cleaned_data = market_data_feed._validate_data(data)
    
    # Verify invalid rows were removed
    assert len(cleaned_data) < len(data)
    
    # Verify price relationships
    assert all(cleaned_data['high'] >= cleaned_data['low'])
    assert all(cleaned_data['high'] >= cleaned_data['open'])
    assert all(cleaned_data['high'] >= cleaned_data['close'])
    assert all(cleaned_data['low'] <= cleaned_data['open'])
    assert all(cleaned_data['low'] <= cleaned_data['close'])
    assert all(cleaned_data['volume'] >= 0)

def test_technical_indicators(market_data_feed):
    """Test technical indicator calculation."""
    # Create test data
    data = pd.DataFrame({
        'symbol': ['AAPL'] * 100,
        'timestamp': pd.date_range(start='2023-01-01', periods=100),
        'open': np.random.normal(100, 1, 100),
        'high': np.random.normal(101, 1, 100),
        'low': np.random.normal(99, 1, 100),
        'close': np.random.normal(100, 1, 100),
        'volume': np.random.normal(1000, 100, 100)
    })
    
    # Add technical indicators
    data_with_indicators = market_data_feed.add_technical_indicators(data)
    
    # Verify indicators were added
    assert 'sma_20' in data_with_indicators.columns
    assert 'sma_50' in data_with_indicators.columns
    assert 'rsi' in data_with_indicators.columns
    assert 'bb_upper' in data_with_indicators.columns
    assert 'bb_middle' in data_with_indicators.columns
    assert 'bb_lower' in data_with_indicators.columns
    
    # Verify indicator calculations
    assert not data_with_indicators['sma_20'].isna().all()
    assert not data_with_indicators['sma_50'].isna().all()
    assert not data_with_indicators['rsi'].isna().all()
    assert not data_with_indicators['bb_upper'].isna().all()
    assert not data_with_indicators['bb_middle'].isna().all()
    assert not data_with_indicators['bb_lower'].isna().all()

def test_multiple_symbols(market_data_feed):
    """Test handling of multiple symbols."""
    end_date = datetime.now()
    start_date = end_date - timedelta(days=7)
    
    # Test multiple symbols
    symbols = ["AAPL", "MSFT", "GOOGL"]
    data = market_data_feed.get_historical_data(symbols, start_date, end_date)
    
    # Verify data contains all symbols
    assert set(data['symbol'].unique()) == set(symbols)
    
    # Verify data is properly grouped
    for symbol in symbols:
        symbol_data = data[data['symbol'] == symbol]
        assert not symbol_data.empty
        assert all(col in symbol_data.columns for col in ['symbol', 'timestamp', 'open', 'high', 'low', 'close', 'volume']) 