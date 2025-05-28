"""Market data feed implementation."""

import os
import asyncio
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Optional, Union, Dict, Any
import alpaca_trade_api as tradeapi
import yfinance as yf
from ta.trend import SMAIndicator
from ta.momentum import RSIIndicator
from ta.volatility import BollingerBands

from ..core.config import config
from ..core.logging import log_manager

logger = log_manager.get_logger(__name__)

class MarketDataFeed:
    """Market data feed that supports multiple data sources and caching.
    
    This class provides a unified interface for accessing market data from
    multiple sources (Alpaca, Yahoo Finance) with automatic caching and
    data validation.
    
    Attributes:
        config: Configuration manager instance.
        logger: Logger instance.
        cache_dir: Directory for caching market data.
        api: Alpaca API client.
        data_sources: Dictionary of available data sources.
    """
    
    def __init__(self, config, logger, cache_dir: str = "data/cache"):
        """Initialize the market data feed.
        
        Args:
            config: Configuration manager instance.
            logger: Logger instance.
            cache_dir: Directory for caching market data.
        """
        self.config = config
        self.logger = logger
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize Alpaca API
        self.api = tradeapi.REST(
            key_id=os.getenv('ALPACA_API_KEY'),
            secret_key=os.getenv('ALPACA_SECRET_KEY'),
            base_url=os.getenv('ALPACA_BASE_URL', 'https://paper-api.alpaca.markets')
        )
        
        # Register data sources
        self.data_sources = {
            'alpaca': self._get_alpaca_data,
            'yfinance': self._get_yfinance_data
        }
        
    def get_historical_data(
        self,
        symbols: Union[str, List[str]],
        start_date: datetime,
        end_date: datetime,
        interval: str = '1d',
        source: str = 'alpaca'
    ) -> pd.DataFrame:
        """Get historical market data.
        
        Args:
            symbols: Single symbol or list of symbols.
            start_date: Start date for data.
            end_date: End date for data.
            interval: Data interval (1m, 5m, 15m, 1h, 1d).
            source: Data source to use ('alpaca' or 'yfinance').
            
        Returns:
            DataFrame containing historical market data.
            
        Raises:
            ValueError: If invalid parameters are provided.
        """
        # Validate parameters
        self._validate_parameters(symbols, start_date, end_date, interval, source)
        
        # Convert single symbol to list
        if isinstance(symbols, str):
            symbols = [symbols]
            
        # Check cache first
        cached_data = self._get_cached_data(symbols, start_date, end_date, interval)
        if cached_data is not None:
            return cached_data
            
        # Fetch data from source
        try:
            data = self.data_sources[source](symbols, start_date, end_date, interval)
            
            # Validate and clean data
            data = self._validate_data(data)
            
            # Cache the data
            self._cache_data(data, symbols, start_date, end_date, interval)
            
            return data
            
        except Exception as e:
            self.logger.error(f"Error fetching market data: {str(e)}")
            raise
            
    def _validate_parameters(
        self,
        symbols: Union[str, List[str]],
        start_date: datetime,
        end_date: datetime,
        interval: str,
        source: str
    ):
        """Validate input parameters.
        
        Args:
            symbols: Symbols to validate.
            start_date: Start date to validate.
            end_date: End date to validate.
            interval: Interval to validate.
            source: Data source to validate.
            
        Raises:
            ValueError: If any parameter is invalid.
        """
        # Validate symbols
        if isinstance(symbols, list):
            if not all(isinstance(s, str) for s in symbols):
                raise ValueError("All symbols must be strings")
        elif not isinstance(symbols, str):
            raise ValueError("Symbols must be a string or list of strings")
            
        # Validate dates
        if not isinstance(start_date, datetime) or not isinstance(end_date, datetime):
            raise ValueError("Start and end dates must be datetime objects")
        if start_date >= end_date:
            raise ValueError("Start date must be before end date")
            
        # Validate interval
        valid_intervals = ['1m', '5m', '15m', '1h', '1d']
        if interval not in valid_intervals:
            raise ValueError(f"Invalid interval. Must be one of {valid_intervals}")
            
        # Validate source
        if source not in self.data_sources:
            raise ValueError(f"Invalid source. Must be one of {list(self.data_sources.keys())}")
            
    def _validate_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """Validate and clean market data.
        
        Args:
            data: DataFrame to validate.
            
        Returns:
            Cleaned DataFrame.
            
        Raises:
            ValueError: If data validation fails.
        """
        # Check for required columns
        required_columns = ['symbol', 'timestamp', 'open', 'high', 'low', 'close', 'volume']
        missing_columns = [col for col in required_columns if col not in data.columns]
        if missing_columns:
            raise ValueError(f"Missing required columns: {missing_columns}")
            
        # Remove rows with missing values
        data = data.dropna()
        
        # Validate price relationships
        price_errors = (
            (data['high'] < data['low']) |
            (data['high'] < data['open']) |
            (data['high'] < data['close']) |
            (data['low'] > data['open']) |
            (data['low'] > data['close']) |
            (data['volume'] < 0)
        )
        
        if price_errors.any():
            self.logger.warning(f"Found {price_errors.sum()} invalid price relationships")
            data = data[~price_errors]
            
        # Sort by timestamp
        data = data.sort_values('timestamp')
        
        return data
        
    def _get_cached_data(
        self,
        symbols: List[str],
        start_date: datetime,
        end_date: datetime,
        interval: str
    ) -> Optional[pd.DataFrame]:
        """Get data from cache if available.
        
        Args:
            symbols: List of symbols.
            start_date: Start date.
            end_date: End date.
            interval: Data interval.
            
        Returns:
            Cached DataFrame if available, None otherwise.
        """
        cache_file = self.cache_dir / f"{'_'.join(symbols)}_{interval}_{start_date.date()}_{end_date.date()}.parquet"
        
        if cache_file.exists():
            try:
                data = pd.read_parquet(cache_file)
                self.logger.info(f"Retrieved data from cache: {cache_file}")
                return data
            except Exception as e:
                self.logger.warning(f"Error reading cache file: {str(e)}")
                
        return None
        
    def _cache_data(
        self,
        data: pd.DataFrame,
        symbols: List[str],
        start_date: datetime,
        end_date: datetime,
        interval: str
    ):
        """Cache market data.
        
        Args:
            data: DataFrame to cache.
            symbols: List of symbols.
            start_date: Start date.
            end_date: End date.
            interval: Data interval.
        """
        cache_file = self.cache_dir / f"{'_'.join(symbols)}_{interval}_{start_date.date()}_{end_date.date()}.parquet"
        
        try:
            data.to_parquet(cache_file)
            self.logger.info(f"Cached data to: {cache_file}")
        except Exception as e:
            self.logger.error(f"Error caching data: {str(e)}")
            
    def _get_alpaca_data(
        self,
        symbols: List[str],
        start_date: datetime,
        end_date: datetime,
        interval: str
    ) -> pd.DataFrame:
        """Get data from Alpaca.
        
        Args:
            symbols: List of symbols.
            start_date: Start date.
            end_date: End date.
            interval: Data interval.
            
        Returns:
            DataFrame containing Alpaca market data.
        """
        dfs = []
        
        for symbol in symbols:
            try:
                bars = self.api.get_bars(
                    symbol,
                    interval,
                    start_date.isoformat(),
                    end_date.isoformat()
                ).df
                
                bars['symbol'] = symbol
                dfs.append(bars)
                
            except Exception as e:
                self.logger.error(f"Error fetching Alpaca data for {symbol}: {str(e)}")
                continue
                
        if not dfs:
            raise ValueError("No data retrieved from Alpaca")
            
        return pd.concat(dfs)
        
    def _get_yfinance_data(
        self,
        symbols: List[str],
        start_date: datetime,
        end_date: datetime,
        interval: str
    ) -> pd.DataFrame:
        """Get data from Yahoo Finance.
        
        Args:
            symbols: List of symbols.
            start_date: Start date.
            end_date: End date.
            interval: Data interval.
            
        Returns:
            DataFrame containing Yahoo Finance market data.
        """
        dfs = []
        
        for symbol in symbols:
            try:
                ticker = yf.Ticker(symbol)
                data = ticker.history(
                    start=start_date,
                    end=end_date,
                    interval=interval
                )
                
                data['symbol'] = symbol
                dfs.append(data)
                
            except Exception as e:
                self.logger.error(f"Error fetching Yahoo Finance data for {symbol}: {str(e)}")
                continue
                
        if not dfs:
            raise ValueError("No data retrieved from Yahoo Finance")
            
        return pd.concat(dfs)
        
    def add_technical_indicators(self, data: pd.DataFrame) -> pd.DataFrame:
        """Add technical indicators to market data.
        
        Args:
            data: DataFrame containing market data.
            
        Returns:
            DataFrame with added technical indicators.
        """
        # Group by symbol to calculate indicators for each symbol
        grouped = data.groupby('symbol')
        
        # Calculate indicators
        sma_20 = grouped.apply(lambda x: SMAIndicator(close=x['close'], window=20).sma_indicator())
        sma_50 = grouped.apply(lambda x: SMAIndicator(close=x['close'], window=50).sma_indicator())
        rsi = grouped.apply(lambda x: RSIIndicator(close=x['close']).rsi())
        bb = grouped.apply(lambda x: BollingerBands(close=x['close']).bollinger_bands())
        
        # Add indicators to DataFrame
        data['sma_20'] = sma_20
        data['sma_50'] = sma_50
        data['rsi'] = rsi
        data['bb_upper'] = bb['bb_upper']
        data['bb_middle'] = bb['bb_middle']
        data['bb_lower'] = bb['bb_lower']
        
        return data 