"""Configuration management for the trading system.

This module provides a centralized configuration management system that handles both
YAML-based configuration files and environment variables. It supports hierarchical
configuration with environment variable overrides.

Example:
    ```python
    from src.core.config import config

    # Get a configuration value
    symbol = config.get('trading.default_symbol')

    # Get a section of configuration
    trading_config = config.get_trading_config()
    ```
"""

import os
from pathlib import Path
from typing import Any, Dict, Optional

import yaml
from dotenv import load_dotenv


class ConfigManager:
    """Manages configuration for the trading system.

    This class handles loading and accessing configuration from both YAML files
    and environment variables. It provides type-safe access to configuration
    values and supports hierarchical configuration with environment variable
    overrides.

    Attributes:
        config_path (str): Path to the YAML configuration file.
        config (Dict[str, Any]): The loaded configuration dictionary.

    Example:
        ```python
        config = ConfigManager('config/config.yaml')
        symbol = config.get('trading.default_symbol')
        ```
    """

    def __init__(self, config_path: str = "config/config.yaml"):
        """Initialize the configuration manager.

        Args:
            config_path: Path to the YAML configuration file. Defaults to
                "config/config.yaml".

        Raises:
            RuntimeError: If the configuration file cannot be loaded or is invalid.
        """
        self.config_path = Path(config_path)
        self.config = self._load_config()
        self._load_env()

    def _load_config(self) -> Dict[str, Any]:
        """Load configuration from YAML file.

        Returns:
            Dictionary containing configuration settings.
            
        Raises:
            RuntimeError: If the configuration file cannot be loaded or is invalid.
        """
        if not self.config_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {self.config_path}")

        try:
            with open(self.config_path, "r") as f:
                return yaml.safe_load(f)
        except yaml.YAMLError as e:
            raise RuntimeError(f"Invalid configuration file: {e}")

    def _load_env(self):
        """Load environment variables.

        This method loads environment variables from a .env file if it exists.
        Environment variables can override configuration values from the YAML file.
        """
        load_dotenv()

    def get(self, key: str, default: Any = None) -> Any:
        """Get a configuration value.

        This method retrieves a configuration value using a dot-notation key
        (e.g., 'trading.default_symbol'). It first checks for an environment
        variable with the same name (in uppercase), then falls back to the
        YAML configuration.

        Args:
            key: The configuration key in dot notation.
            default: The default value to return if the key is not found.

        Returns:
            The configuration value, or the default value if not found.

        Example:
            ```python
            # Get a configuration value
            symbol = config.get('trading.default_symbol', 'AAPL')

            # Get a nested value
            window = config.get('strategy.sma_crossover.short_window', 20)
            ```
        """
        # Try environment variable first
        # Convert dots to underscores for environment variable lookup
        env_key = key.upper().replace(".", "_")
        env_value = os.getenv(env_key)
        if env_value is not None:
            return env_value

        # Then try config file
        keys = key.split(".")
        value = self.config
        for k in keys:
            if isinstance(value, dict):
                value = value.get(k, default)
            else:
                return default
        return value if value is not None else default

    def get_trading_config(self) -> Dict[str, Any]:
        """Get trading configuration.

        Returns:
            Dictionary containing trading configuration.

        Example:
            ```python
            trading_config = config.get_trading_config()
            symbol = trading_config['default_symbol']
            position_size = trading_config['position_size']
            ```
        """
        return self.config.get("trading", {})

    def get_market_data_config(self) -> Dict[str, Any]:
        """Get market data configuration.

        Returns:
            Dictionary containing market data configuration.

        Example:
            ```python
            market_config = config.get_market_data_config()
            cache_dir = market_config['cache_dir']
            update_interval = market_config['update_interval']
            ```
        """
        return self.config.get("market_data", {})

    def get_strategy_config(self, strategy_name: str) -> Dict[str, Any]:
        """Get configuration for a specific strategy.

        Args:
            strategy_name: Name of the strategy to get configuration for.

        Returns:
            Dictionary containing strategy configuration.

        Example:
            ```python
            sma_config = config.get_strategy_config('sma_crossover')
            short_window = sma_config['short_window']
            long_window = sma_config['long_window']
            ```
        """
        return self.config.get("strategy", {}).get(strategy_name, {})

    def get_logging_config(self) -> Dict[str, Any]:
        """Get logging configuration.

        Returns:
            Dictionary containing logging configuration.

        Example:
            ```python
            log_config = config.get_logging_config()
            level = log_config['level']
            file_path = log_config['file_path']
            ```
        """
        return self.config.get("logging", {})

    def get_database_config(self) -> Dict[str, Any]:
        """Get database configuration.

        Returns:
            Dictionary containing database configuration.

        Example:
            ```python
            db_config = config.get_database_config()
            db_path = db_config['path']
            ```
        """
        return self.config.get("database", {})

    def get_performance_config(self) -> Dict[str, Any]:
        """Get performance configuration.

        Returns:
            Dictionary containing performance configuration.

        Example:
            ```python
            perf_config = config.get_performance_config()
            max_daily_loss = perf_config['max_daily_loss']
            max_position_size = perf_config['max_position_size']
            ```
        """
        return self.config.get("performance", {})

    def set(self, key: str, value: Any):
        """Set a configuration value.

        Args:
            key: Configuration key (dot notation supported).
            value: Value to set.
        """
        keys = key.split(".")
        config = self.config

        for k in keys[:-1]:
            if k not in config:
                config[k] = {}
            config = config[k]

        config[keys[-1]] = value

    def save(self):
        """Save configuration to file."""
        with open(self.config_path, "w") as f:
            yaml.dump(self.config, f, default_flow_style=False)


# Global config manager instance will be created when needed
_config_manager: Optional[ConfigManager] = None


def get_config_manager(config_path: str = "config/config.yaml") -> ConfigManager:
    """Get or create the global config manager instance.

    Args:
        config_path: Path to the configuration file.

    Returns:
        ConfigManager instance.
    """
    global _config_manager
    if _config_manager is None:
        _config_manager = ConfigManager(config_path)
    return _config_manager
