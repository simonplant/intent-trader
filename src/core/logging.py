"""Logging configuration for the trading system."""

import logging
import logging.handlers
import os
from pathlib import Path
from typing import Optional

from .config import config

class LogManager:
    """Manages logging configuration for the trading system."""
    
    def __init__(self):
        """Initialize the logging manager."""
        self.logger = logging.getLogger('intent_trader')
        self._setup_logging()
        
    def _setup_logging(self):
        """Set up logging configuration."""
        log_config = config.get_logging_config()
        
        # Set log level
        log_level = getattr(logging, log_config.get('level', 'INFO'))
        self.logger.setLevel(log_level)
        
        # Create logs directory if it doesn't exist
        log_dir = Path(log_config.get('file_path', 'logs')).parent
        log_dir.mkdir(parents=True, exist_ok=True)
        
        # File handler with rotation
        file_handler = logging.handlers.RotatingFileHandler(
            filename=log_config.get('file_path', 'logs/trading.log'),
            maxBytes=log_config.get('max_size', 10 * 1024 * 1024),  # 10MB
            backupCount=log_config.get('backup_count', 5)
        )
        
        # Console handler
        console_handler = logging.StreamHandler()
        
        # Create formatter
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        
        # Add formatter to handlers
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        # Add handlers to logger
        self.logger.addHandler(file_handler)
        self.logger.addHandler(console_handler)
        
    def get_logger(self, name: Optional[str] = None) -> logging.Logger:
        """Get a logger instance.
        
        Args:
            name: Optional name for the logger. If None, returns the main logger.
            
        Returns:
            A configured logger instance.
        """
        if name:
            return self.logger.getChild(name)
        return self.logger

# Create a global log manager instance
log_manager = LogManager() 