#!/usr/bin/env python3
"""Run the first test of the trading system."""

import logging
import os
import sys
from pathlib import Path

# Add src to Python path
src_path = Path(__file__).parent.parent / "src"
sys.path.append(str(src_path))

from market_data.feed import MarketDataFeed
from order.manager import OrderManager, OrderSide, OrderType
from strategy.engine import StrategyEngine

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def main():
    """Run the first test of the trading system."""
    logger.info("Starting first test run...")

    try:
        # Initialize components
        feed = MarketDataFeed()
        engine = StrategyEngine()
        manager = OrderManager()

        # Test market data
        logger.info("Testing market data feed...")
        data = feed.get_historical_data("AAPL", period="1d", interval="1m")
        logger.info(f"Retrieved {len(data)} data points")

        # Test strategy
        logger.info("Testing strategy engine...")
        strategy_config = {"short_window": 20, "long_window": 50}
        engine.add_strategy("sma_crossover", strategy_config)

        # Generate signals
        signals = engine.generate_signals(data)
        logger.info(f"Generated signals: {signals}")

        # Test order management
        logger.info("Testing order management...")
        if signals["sma_crossover"]["signal"] == 1:
            order = manager.create_order(
                symbol="AAPL",
                side=OrderSide.BUY,
                order_type=OrderType.MARKET,
                quantity=100,
            )
            logger.info(f"Created order: {order.to_dict()}")

        logger.info("First test completed successfully!")

    except Exception as e:
        logger.error(f"Error during test: {str(e)}")
        sys.exit(1)


if __name__ == "__main__":
    main()
