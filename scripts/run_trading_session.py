import pandas as pd
from src.market_data.feed import MarketDataFeed
from src.strategy.engine import StrategyEngine
from src.order.manager import OrderManager, OrderSide, OrderType

# Initialize components
market_data = MarketDataFeed()
strategy_engine = StrategyEngine()
order_manager = OrderManager()

# Get market data
data = market_data.get_historical_data("AAPL", period="30d", interval="1d")

# Convert data to DataFrame
data_df = pd.DataFrame(data)

# Add 'symbol' column if missing
if 'symbol' not in data_df.columns:
    data_df['symbol'] = 'AAPL'

# Map 'Close' to 'close' if necessary
if 'Close' in data_df.columns and 'close' not in data_df.columns:
    data_df['close'] = data_df['Close']

# Generate trading signals
signals = strategy_engine.generate_signals(
    data=data_df,
    strategy="sma_crossover",
    params={"short_window": 20, "long_window": 50}
)

# Iterate over signals
for index, signal in signals.iterrows():
    if signal["signal"] == 1:  # Buy signal
        order = order_manager.create_order(
            symbol=signal["symbol"],
            side=OrderSide.BUY,
            order_type=OrderType.MARKET,
            quantity=100
        )