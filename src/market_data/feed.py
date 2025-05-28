"""Market data feed module for real-time and historical data."""

import yfinance as yf
from datetime import datetime, timedelta
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class MarketDataFeed:
    def __init__(self):
        self.active_symbols = set()
        self.data_cache = {}
        
    def get_historical_data(self, symbol: str, period: str = "1d", interval: str = "1m") -> dict:
        """Fetch historical data for a symbol."""
        try:
            ticker = yf.Ticker(symbol)
            data = ticker.history(period=period, interval=interval)
            return data.to_dict('records')
        except Exception as e:
            logger.error(f"Error fetching historical data for {symbol}: {str(e)}")
            return {}
            
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
        try:
            ticker = yf.Ticker(symbol)
            return ticker.info.get('regularMarketPrice', 0.0)
        except Exception as e:
            logger.error(f"Error fetching latest price for {symbol}: {str(e)}")
            return 0.0

if __name__ == "__main__":
    # Test the feed
    feed = MarketDataFeed()
    
    # Test historical data
    data = feed.get_historical_data("AAPL", period="1d", interval="1m")
    print(f"Historical data points: {len(data)}")
    
    # Test real-time price
    price = feed.get_latest_price("AAPL")
    print(f"Latest AAPL price: ${price}") 