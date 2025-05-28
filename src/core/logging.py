"""Logging manager for the trading system."""

import os
import logging
from logging.handlers import RotatingFileHandler
from pathlib import Path
from typing import Optional

class LogManager:
    """Manages logging configuration and provides logger instances."""
    
    def __init__(self, config):
        """Initialize the logging manager.
        
        Args:
            config: Configuration manager instance.
        """
        self.config = config
        self.loggers = {}
        self._setup_root_logger()
        
    def _setup_root_logger(self):
        """Set up the root logger with file and console handlers."""
        log_config = self.config.get('logging', {})
        log_level = getattr(logging, log_config.get('level', 'INFO'))
        log_file = log_config.get('file', 'logs/trading.log')
        max_size = log_config.get('max_size', 10 * 1024 * 1024)  # 10MB
        backup_count = log_config.get('backup_count', 5)
        
        # Create logs directory if it doesn't exist
        log_path = Path(log_file)
        log_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Configure root logger
        root_logger = logging.getLogger()
        root_logger.setLevel(log_level)
        
        # File handler
        file_handler = RotatingFileHandler(
            log_file,
            maxBytes=max_size,
            backupCount=backup_count
        )
        file_handler.setFormatter(
            logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )
        root_logger.addHandler(file_handler)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setFormatter(
            logging.Formatter('%(levelname)s: %(message)s')
        )
        root_logger.addHandler(console_handler)
        
    def get_logger(self, name: str) -> logging.Logger:
        """Get a logger instance.
        
        Args:
            name: Logger name.
            
        Returns:
            Logger instance.
        """
        if name not in self.loggers:
            self.loggers[name] = logging.getLogger(name)
        return self.loggers[name]

# Global log manager instance will be created when needed
_log_manager: Optional[LogManager] = None

def get_log_manager(config=None) -> LogManager:
    """Get or create the global log manager instance.
    
    Args:
        config: Configuration manager instance (required for first call).
        
    Returns:
        LogManager instance.
    """
    global _log_manager
    if _log_manager is None:
        if config is None:
            # Create a minimal config for basic logging
            class MinimalConfig:
                def get(self, key, default=None):
                    return default
            config = MinimalConfig()
        _log_manager = LogManager(config)
    return _log_manager 