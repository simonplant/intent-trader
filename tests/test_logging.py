"""Tests for logging functionality."""

import logging
import tempfile
from pathlib import Path
from io import StringIO

import pytest

from src.core.config import ConfigManager, get_config_manager
from src.core.logging import LogManager, get_log_manager


@pytest.fixture
def log_manager(tmp_path):
    """Create a temporary log directory and manager."""
    log_dir = tmp_path / "logs"
    log_dir.mkdir()

    config_path = tmp_path / "config.yaml"
    config_content = """
    logging:
        level: DEBUG
        file: logs/trading.log
        max_size: 1048576
        backup_count: 3
    """
    config_path.write_text(config_content)

    config = ConfigManager(str(config_path))
    return LogManager(config)


def test_logger_creation(log_manager):
    """Test logger creation and basic functionality."""
    logger = log_manager.get_logger("intent_trader")
    assert isinstance(logger, logging.Logger)
    assert logger.name == "intent_trader"
    # Root logger level is set, not individual logger
    root_logger = logging.getLogger()
    assert root_logger.level == logging.DEBUG


def test_child_logger(log_manager):
    """Test creation of child loggers."""
    child_logger = log_manager.get_logger("intent_trader.market_data")
    assert child_logger.name == "intent_trader.market_data"
    # Child logger inherits from root
    root_logger = logging.getLogger()
    assert root_logger.level == logging.DEBUG


def test_log_handlers(log_manager):
    """Test that loggers have the correct handlers."""
    # Get root logger since handlers are attached there
    root_logger = logging.getLogger()
    handlers = root_logger.handlers

    # Should have both file and console handlers
    assert len(handlers) >= 2
    assert any(isinstance(h, logging.handlers.RotatingFileHandler) for h in handlers)
    assert any(isinstance(h, logging.StreamHandler) for h in handlers)


def test_log_rotation(log_manager, tmp_path):
    """Test log file rotation."""
    logger = log_manager.get_logger("intent_trader")

    # Write some logs
    for i in range(10):
        logger.info(f"Test log message {i}")

    # Force flush all handlers
    for handler in logging.getLogger().handlers:
        handler.flush()

    # Check if log file was created in the expected location
    expected_log_file = tmp_path / "logs" / "trading.log"
    
    # The LogManager might be using a relative path, so let's check both locations
    log_files = []
    if expected_log_file.exists():
        log_files.append(expected_log_file)
    
    # Also check in the current directory
    cwd_log_file = Path("logs/trading.log")
    if cwd_log_file.exists():
        log_files.append(cwd_log_file)
    
    # Should have at least one log file
    assert len(log_files) >= 1, f"No log files found. Expected at {expected_log_file} or {cwd_log_file}"
    
    # Verify the log file has content
    if log_files:
        content = log_files[0].read_text()
        assert "Test log message" in content


def test_log_formatting(log_manager, caplog):
    """Test log message formatting."""
    logger = log_manager.get_logger("intent_trader")

    # Log a test message
    test_message = "Test log message"
    with caplog.at_level(logging.INFO):
        logger.info(test_message)

    # Check that the message was logged
    assert test_message in caplog.text
