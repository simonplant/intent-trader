"""Strategy engine implementation."""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Union
from datetime import datetime

from ..core.config import config
from ..core.logging import log_manager

logger = log_manager.get_logger(__name__)

class StrategyEngine:
    """Engine for generating trading signals from market data."""
    
    def __init__(self):
        """Initialize the strategy engine."""
        self.config = config
        self.strategies = {
            'sma_crossover': self._sma_crossover_strategy,
            'rsi': self._rsi_strategy
        }
        
    def generate_signals(
        self,
        data: pd.DataFrame,
        strategy: str,
        params: Optional[Dict] = None
    ) -> pd.DataFrame:
        """Generate trading signals using the specified strategy.
        
        Args:
            data: Market data DataFrame.
            strategy: Strategy name.
            params: Optional strategy parameters.
            
        Returns:
            DataFrame with added signal columns.
        """
        if strategy not in self.strategies:
            raise ValueError(f"Unknown strategy: {strategy}")
            
        try:
            return self.strategies[strategy](data, params)
        except Exception as e:
            logger.error(f"Error generating signals: {str(e)}")
            raise
            
    def _sma_crossover_strategy(
        self,
        data: pd.DataFrame,
        params: Optional[Dict] = None
    ) -> pd.DataFrame:
        """Simple Moving Average Crossover strategy.
        
        Generates buy signals when short SMA crosses above long SMA,
        and sell signals when short SMA crosses below long SMA.
        """
        if params is None:
            params = self.config.get_strategy_config('sma_crossover')
            
        short_window = params.get('short_window', 20)
        long_window = params.get('long_window', 50)
        
        # Calculate SMAs
        data['sma_short'] = data.groupby('symbol')['close'].transform(
            lambda x: x.rolling(window=short_window).mean()
        )
        data['sma_long'] = data.groupby('symbol')['close'].transform(
            lambda x: x.rolling(window=long_window).mean()
        )
        
        # Generate signals
        data['signal'] = 0
        data.loc[data['sma_short'] > data['sma_long'], 'signal'] = 1
        data.loc[data['sma_short'] < data['sma_long'], 'signal'] = -1
        
        # Only generate signals on crossovers
        data['signal'] = data.groupby('symbol')['signal'].diff()
        
        return data
        
    def _rsi_strategy(
        self,
        data: pd.DataFrame,
        params: Optional[Dict] = None
    ) -> pd.DataFrame:
        """Relative Strength Index strategy.
        
        Generates buy signals when RSI crosses below oversold threshold,
        and sell signals when RSI crosses above overbought threshold.
        """
        if params is None:
            params = self.config.get_strategy_config('rsi')
            
        period = params.get('period', 14)
        overbought = params.get('overbought', 70)
        oversold = params.get('oversold', 30)
        
        # Calculate RSI
        delta = data.groupby('symbol')['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        data['rsi'] = 100 - (100 / (1 + rs))
        
        # Generate signals
        data['signal'] = 0
        data.loc[data['rsi'] < oversold, 'signal'] = 1
        data.loc[data['rsi'] > overbought, 'signal'] = -1
        
        # Only generate signals on threshold crossings
        data['signal'] = data.groupby('symbol')['signal'].diff()
        
        return data 