"""Integration tests for market data functionality."""

import pytest
import pandas as pd
from datetime import datetime, timedelta
from pathlib import Path
from src.core.config import get_config_manager
from src.core.logging import LogManager
from src.market_data.feed import MarketDataFeed

@pytest.fixture
def market_data_feed(tmp_path):
    """Create a market data feed for testing."""
    # Set up test configuration
    config_path = tmp_path / "config.yaml"
    config_content = """
    market_data:
        cache_dir: data/cache
        update_interval: 60
        symbols:
            - AAPL
            - MSFT
            - GOOGL
    """
    config_path.write_text(config_content)
    
    # Create cache directory
    cache_dir = tmp_path / "data" / "cache"
    cache_dir.mkdir(parents=True)
    
    config = get_config_manager(str(config_path))
    logger = LogManager().get_logger('market_data')
    
    return MarketDataFeed(config, logger, cache_dir=str(cache_dir))

@pytest.mark.integration
def test_market_data_retrieval(market_data_feed):
    """Test retrieving market data for multiple symbols."""
    # Get data for multiple symbols
    symbols = ['AAPL', 'MSFT', 'GOOGL']
    end_date = datetime.now()
    start_date = end_date - timedelta(days=7)
    
    data = market_data_feed.get_historical_data(
        symbols=symbols,
        start_date=start_date,
        end_date=end_date,
        interval='1d'
    )
    
    # Verify data structure
    assert isinstance(data, pd.DataFrame)
    assert not data.empty
    assert all(symbol in data['symbol'].unique() for symbol in symbols)
    assert all(col in data.columns for col in ['open', 'high', 'low', 'close', 'volume'])

@pytest.mark.integration
def test_data_caching(market_data_feed, tmp_path):
    """Test that market data is properly cached."""
    symbol = 'AAPL'
    end_date = datetime.now()
    start_date = end_date - timedelta(days=1)
    
    # First request should fetch from API
    data1 = market_data_feed.get_historical_data(
        symbols=[symbol],
        start_date=start_date,
        end_date=end_date,
        interval='1m'
    )
    
    # Second request should use cache
    data2 = market_data_feed.get_historical_data(
        symbols=[symbol],
        start_date=start_date,
        end_date=end_date,
        interval='1m'
    )
    
    # Verify data consistency
    pd.testing.assert_frame_equal(data1, data2)
    
    # Verify cache file exists
    cache_file = Path(market_data_feed.cache_dir) / f"{symbol}_1m_{start_date.date()}_{end_date.date()}.parquet"
    assert cache_file.exists()

@pytest.mark.integration
def test_data_validation(market_data_feed):
    """Test market data validation."""
    symbol = 'AAPL'
    end_date = datetime.now()
    start_date = end_date - timedelta(days=1)
    
    data = market_data_feed.get_historical_data(
        symbols=[symbol],
        start_date=start_date,
        end_date=end_date,
        interval='1m'
    )
    
    # Verify data quality
    assert not data.isnull().any().any()  # No missing values
    assert (data['high'] >= data['low']).all()  # High >= Low
    assert (data['high'] >= data['open']).all()  # High >= Open
    assert (data['high'] >= data['close']).all()  # High >= Close
    assert (data['low'] <= data['open']).all()  # Low <= Open
    assert (data['low'] <= data['close']).all()  # Low <= Close
    assert (data['volume'] >= 0).all()  # Volume >= 0

@pytest.mark.integration
def test_error_handling(market_data_feed):
    """Test error handling for invalid requests."""
    # Test invalid symbol
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data(
            symbols=['INVALID_SYMBOL'],
            start_date=datetime.now() - timedelta(days=1),
            end_date=datetime.now(),
            interval='1m'
        )
    
    # Test invalid date range
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data(
            symbols=['AAPL'],
            start_date=datetime.now(),
            end_date=datetime.now() - timedelta(days=1),
            interval='1m'
        )
    
    # Test invalid interval
    with pytest.raises(ValueError):
        market_data_feed.get_historical_data(
            symbols=['AAPL'],
            start_date=datetime.now() - timedelta(days=1),
            end_date=datetime.now(),
            interval='invalid_interval'
        )

@pytest.mark.integration
def test_concurrent_requests(market_data_feed):
    """Test handling of concurrent data requests."""
    import asyncio
    
    async def fetch_data(symbol):
        return market_data_feed.get_historical_data(
            symbols=[symbol],
            start_date=datetime.now() - timedelta(days=1),
            end_date=datetime.now(),
            interval='1m'
        )
    
    # Create multiple concurrent requests
    symbols = ['AAPL', 'MSFT', 'GOOGL']
    tasks = [fetch_data(symbol) for symbol in symbols]
    
    # Run requests concurrently
    results = asyncio.run(asyncio.gather(*tasks))
    
    # Verify all requests completed successfully
    assert len(results) == len(symbols)
    assert all(isinstance(df, pd.DataFrame) for df in results)
    assert all(not df.empty for df in results) 