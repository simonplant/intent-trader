from abc import ABC, abstractmethod
from datetime import datetime, time
from typing import Any, Dict, List, Optional

import pytz

from data.schemas import MarketDataSchema


class MarketDataProvider(ABC):
    """Abstract base class for market data providers"""

    @abstractmethod
    async def get_market_data(self, symbols: List[str]) -> Dict[str, MarketDataSchema]:
        """Get market data for specified symbols"""
        pass

    @abstractmethod
    async def get_historical_data(
        self, symbol: str, start_time: datetime, end_time: datetime
    ) -> List[MarketDataSchema]:
        """Get historical market data for a symbol"""
        pass

    @abstractmethod
    async def subscribe(self, symbols: List[str], callback) -> None:
        """Subscribe to real-time updates for symbols"""
        pass

    @abstractmethod
    async def unsubscribe(self, symbols: List[str]) -> None:
        """Unsubscribe from real-time updates for symbols"""
        pass


class MarketHours:
    """Market hours management"""

    def __init__(self):
        self.ny_tz = pytz.timezone("America/New_York")
        self.regular_start = time(9, 30)  # 9:30 AM ET
        self.regular_end = time(16, 0)  # 4:00 PM ET
        self.pre_market_start = time(4, 0)  # 4:00 AM ET
        self.pre_market_end = time(9, 30)  # 9:30 AM ET
        self.post_market_start = time(16, 0)  # 4:00 PM ET
        self.post_market_end = time(20, 0)  # 8:00 PM ET

    def is_market_open(self, current_time: Optional[datetime] = None) -> bool:
        """Check if market is currently open"""
        if current_time is None:
            current_time = datetime.now(self.ny_tz)

        current_time = current_time.astimezone(self.ny_tz)
        current_time_only = current_time.time()

        # Check if it's a weekday
        if current_time.weekday() >= 5:  # 5 = Saturday, 6 = Sunday
            return False

        # Check regular market hours
        if self.regular_start <= current_time_only <= self.regular_end:
            return True

        # Check pre-market hours
        if self.pre_market_start <= current_time_only <= self.pre_market_end:
            return True

        # Check post-market hours
        if self.post_market_start <= current_time_only <= self.post_market_end:
            return True

        return False

    def get_market_session(self, current_time: Optional[datetime] = None) -> str:
        """Get current market session"""
        if not self.is_market_open(current_time):
            return "closed"

        if current_time is None:
            current_time = datetime.now(self.ny_tz)

        current_time = current_time.astimezone(self.ny_tz)
        current_time_only = current_time.time()

        if self.pre_market_start <= current_time_only <= self.pre_market_end:
            return "pre_market"
        elif self.regular_start <= current_time_only <= self.regular_end:
            return "regular"
        elif self.post_market_start <= current_time_only <= self.post_market_end:
            return "post_market"
        else:
            return "closed"


class MarketDataCache:
    """Cache for market data to reduce API calls"""

    def __init__(self, ttl_seconds: int = 60):
        self.cache: Dict[str, Dict[str, Any]] = {}
        self.ttl_seconds = ttl_seconds

    def get(self, symbol: str) -> Optional[MarketDataSchema]:
        """Get cached market data for symbol"""
        if symbol not in self.cache:
            return None

        cache_entry = self.cache[symbol]
        if (datetime.now() - cache_entry["timestamp"]).total_seconds() > self.ttl_seconds:
            del self.cache[symbol]
            return None

        return cache_entry["data"]

    def set(self, symbol: str, data: MarketDataSchema) -> None:
        """Set market data in cache"""
        self.cache[symbol] = {"data": data, "timestamp": datetime.now()}

    def clear(self) -> None:
        """Clear the cache"""
        self.cache.clear()


class MarketDataValidator:
    """Validator for market data"""

    @staticmethod
    def validate_market_data(data: MarketDataSchema) -> bool:
        """Validate market data"""
        try:
            # Check required fields
            if not all([data.symbol, data.price, data.timestamp]):
                return False

            # Check price fields
            if data.price <= 0 or data.high <= 0 or data.low <= 0:
                return False

            # Check high/low relationship
            if data.high < data.low:
                return False

            # Check price is within high/low range
            if not (data.low <= data.price <= data.high):
                return False

            # Check volume
            if data.volume < 0:
                return False

            return True

        except Exception:
            return False


class MarketDataManager:
    """Manager for market data operations"""

    def __init__(self, provider: MarketDataProvider, cache_ttl: int = 60):
        self.provider = provider
        self.cache = MarketDataCache(ttl_seconds=cache_ttl)
        self.market_hours = MarketHours()
        self.validator = MarketDataValidator()
        self.subscribers: Dict[str, List[callable]] = {}

    async def get_market_data(self, symbols: List[str]) -> Dict[str, MarketDataSchema]:
        """Get market data for symbols with caching"""
        result = {}

        # Check cache first
        for symbol in symbols:
            cached_data = self.cache.get(symbol)
            if cached_data:
                result[symbol] = cached_data
                continue

        # Get remaining symbols from provider
        remaining_symbols = [s for s in symbols if s not in result]
        if remaining_symbols:
            new_data = await self.provider.get_market_data(remaining_symbols)

            # Validate and cache new data
            for symbol, data in new_data.items():
                if self.validator.validate_market_data(data):
                    self.cache.set(symbol, data)
                    result[symbol] = data

        return result

    async def subscribe(self, symbols: List[str], callback: callable) -> None:
        """Subscribe to market data updates"""
        # Register callback
        for symbol in symbols:
            if symbol not in self.subscribers:
                self.subscribers[symbol] = []
            self.subscribers[symbol].append(callback)

        # Subscribe to provider
        await self.provider.subscribe(symbols, self._handle_update)

    async def unsubscribe(self, symbols: List[str], callback: callable) -> None:
        """Unsubscribe from market data updates"""
        # Remove callback
        for symbol in symbols:
            if symbol in self.subscribers:
                self.subscribers[symbol].remove(callback)
                if not self.subscribers[symbol]:
                    del self.subscribers[symbol]

        # Unsubscribe from provider
        await self.provider.unsubscribe(symbols)

    async def _handle_update(self, symbol: str, data: MarketDataSchema) -> None:
        """Handle market data update"""
        if self.validator.validate_market_data(data):
            self.cache.set(symbol, data)

            # Notify subscribers
            if symbol in self.subscribers:
                for callback in self.subscribers[symbol]:
                    await callback(symbol, data)

    def is_market_open(self) -> bool:
        """Check if market is currently open"""
        return self.market_hours.is_market_open()

    def get_market_session(self) -> str:
        """Get current market session"""
        return self.market_hours.get_market_session()
