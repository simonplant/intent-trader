"""Tests for configuration management."""

import pytest
import tempfile
from pathlib import Path
from src.core.config import ConfigManager, get_config_manager

@pytest.fixture
def config_manager(tmp_path):
    """Create a temporary config file and manager."""
    config_path = tmp_path / "config.yaml"
    config_content = """
    trading:
        default_symbol: AAPL
        position_size: 100
        max_positions: 5
        risk_per_trade: 0.02
    
    market_data:
        cache_dir: data/cache
        update_interval: 60
        symbols:
            - AAPL
            - MSFT
            - GOOGL
    
    strategy:
        sma_crossover:
            short_window: 20
            long_window: 50
        rsi:
            period: 14
            overbought: 70
            oversold: 30
    
    logging:
        level: INFO
        file_path: logs/trading.log
        max_size: 10485760
        backup_count: 5
    
    database:
        path: data/db/trading.db
    
    performance:
        max_daily_loss: 0.05
        max_position_size: 1000
        max_open_trades: 10
    """
    config_path.write_text(config_content)
    return ConfigManager(str(config_path))

def test_load_config(config_manager):
    """Test loading configuration from file."""
    trading_config = config_manager.get_trading_config()
    assert trading_config['default_symbol'] == 'AAPL'
    assert trading_config['position_size'] == 100
    assert trading_config['max_positions'] == 5
    assert trading_config['risk_per_trade'] == 0.02

def test_get_strategy_config(config_manager):
    """Test getting strategy configuration."""
    sma_config = config_manager.get_strategy_config('sma_crossover')
    assert sma_config['short_window'] == 20
    assert sma_config['long_window'] == 50

    rsi_config = config_manager.get_strategy_config('rsi')
    assert rsi_config['period'] == 14
    assert rsi_config['overbought'] == 70
    assert rsi_config['oversold'] == 30

def test_get_nested_config(config_manager):
    """Test getting nested configuration values."""
    assert config_manager.get('trading.default_symbol') == 'AAPL'
    assert config_manager.get('market_data.symbols') == ['AAPL', 'MSFT', 'GOOGL']
    assert config_manager.get('nonexistent.key', 'default') == 'default'

def test_environment_variables(config_manager, monkeypatch):
    """Test environment variable override."""
    monkeypatch.setenv('TRADING_DEFAULT_SYMBOL', 'MSFT')
    assert config_manager.get('trading.default_symbol') == 'MSFT'

def test_invalid_config_file(tmp_path):
    """Test handling of invalid config file."""
    config_path = tmp_path / "invalid.yaml"
    config_path.write_text("invalid: yaml: content: [")
    
    with pytest.raises(RuntimeError):
        ConfigManager(str(config_path)) 