"""Market data feed module for real-time and historical data."""

import logging
import random
from datetime import datetime, timedelta
from typing import Dict, List, Optional

# Make yfinance optional
try:
    import yfinance as yf

    YFINANCE_AVAILABLE = True
except ImportError:
    YFINANCE_AVAILABLE = False
    yf = None

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class MarketDataFeed:
    def __init__(self):
        self.active_symbols = set()
        self.data_cache = {}
        self.mock_prices = {
            "AAPL": 225.50,
            "MSFT": 415.30,
            "GOOGL": 175.80,
            "TSLA": 248.90,
            "NVDA": 875.20,
            "SPY": 485.60,
            "QQQ": 395.40,
        }

    def get_historical_data(self, symbol: str, period: str = "1d", interval: str = "1m") -> dict:
        """Fetch historical data for a symbol."""
        if YFINANCE_AVAILABLE:
            try:
                ticker = yf.Ticker(symbol)
                data = ticker.history(period=period, interval=interval)
                if not data.empty:
                    return data.to_dict("records")
            except Exception as e:
                logger.error(f"Error fetching historical data for {symbol}: {str(e)}")

        # Fallback to mock data
        logger.info(f"Using mock historical data for {symbol}")
        return self._generate_mock_historical_data(symbol)

    def _generate_mock_historical_data(self, symbol: str) -> List[Dict]:
        """Generate mock historical data for testing."""
        base_price = self.mock_prices.get(symbol, 100.0)
        data = []

        for i in range(10):  # Generate 10 data points
            price_change = random.uniform(-0.02, 0.02)  # ±2% change
            price = base_price * (1 + price_change)

            data.append(
                {
                    "Open": price * 0.999,
                    "High": price * 1.005,
                    "Low": price * 0.995,
                    "Close": price,
                    "Volume": random.randint(100000, 1000000),
                    "Datetime": datetime.now() - timedelta(minutes=10 - i),
                }
            )

        return data

    def start_realtime_feed(self, symbol: str):
        """Start real-time data feed for a symbol."""
        if symbol not in self.active_symbols:
            self.active_symbols.add(symbol)
            logger.info(f"Started real-time feed for {symbol}")

    def stop_realtime_feed(self, symbol: str):
        """Stop real-time data feed for a symbol."""
        if symbol in self.active_symbols:
            self.active_symbols.remove(symbol)
            logger.info(f"Stopped real-time feed for {symbol}")

    def get_latest_price(self, symbol: str) -> float:
        """Get latest price for a symbol."""
        if YFINANCE_AVAILABLE:
            try:
                ticker = yf.Ticker(symbol)
                info = ticker.info
                # Try different price fields
                price = (
                    info.get("regularMarketPrice")
                    or info.get("currentPrice")
                    or info.get("previousClose")
                )
                if price and price > 0:
                    return float(price)
            except Exception as e:
                logger.warning(f"Error fetching latest price for {symbol}: {str(e)}")

        # Fallback to mock price with small random variation
        base_price = self.mock_prices.get(symbol, 100.0)
        variation = random.uniform(-0.01, 0.01)  # ±1% variation
        price = base_price * (1 + variation)
        logger.info(f"Using mock price for {symbol}: ${price:.2f}")
        return price

    def get_multiple_prices(self, symbols: List[str]) -> Dict[str, float]:
        """Get latest prices for multiple symbols."""
        prices = {}
        for symbol in symbols:
            prices[symbol] = self.get_latest_price(symbol)
        return prices


if __name__ == "__main__":
    # Test the feed
    feed = MarketDataFeed()

    # Test historical data
    data = feed.get_historical_data("AAPL", period="1d", interval="1m")
    print(f"Historical data points: {len(data)}")

    # Test real-time price
    price = feed.get_latest_price("AAPL")
    print(f"Latest AAPL price: ${price}")

    # Test multiple prices
    prices = feed.get_multiple_prices(["AAPL", "MSFT", "GOOGL"])
    print(f"Multiple prices: {prices}")
