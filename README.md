# Intent Trader

A lightweight, efficient algorithmic trading system designed for solo traders. Built with simplicity and performance in mind, focusing on what matters most for individual trading strategies.

## Features

- **Streamlined Order Management**
  - Fast order execution
  - Simple position tracking
  - Essential risk management
  - SQLite-based trade history

- **Strategy Engine**
  - Single-strategy focus
  - Real-time signal generation
  - Basic performance tracking
  - Minimal risk parameters

- **Market Data System**
  - Real-time market data feed
  - Local data caching
  - Simple data validation
  - SQLite storage

- **Performance Analysis**
  - Key metrics tracking
  - Basic risk analysis
  - Simple performance reports
  - Essential visualizations

## System Architecture

The system follows a minimal, efficient architecture designed for solo traders. For detailed information about the system architecture, please refer to [ARCHITECTURE.md](ARCHITECTURE.md).

## Installation

For detailed installation instructions, please refer to [INSTALL.md](INSTALL.md).

### Quick Start

1. Clone the repository:
```bash
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader
```

2. Set up the environment:
```bash
# Install pyenv if not already installed
brew install pyenv

# Install Python 3.13.3 via pyenv
pyenv install 3.13.3
pyenv local 3.13.3    # set local version for this project

# Create and activate virtual environment
python -m venv venv
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt
```

3. Configure the environment:
```bash
cp .env.example .env
# Edit .env with your configuration
```

4. Initialize the system:
```bash
python scripts/init_db.py
```

## Usage

### Starting the System

1. Start the market data feed:
```bash
python src/market_data/feed.py
```

2. Start the strategy engine:
```bash
python src/strategy/engine.py
```

3. Start the order management system:
```bash
python src/order/manager.py
```

### Configuration

- Strategy configuration: `config/strategy.yaml`
- Risk management: `config/risk.yaml`
- System monitoring: `config/monitoring.yaml`

## Documentation

- [System Architecture](ARCHITECTURE.md)
- [Installation Guide](INSTALL.md)
- [Strategy Guide](docs/strategy_guide.md)

## Development

### Prerequisites

- macOS 10.15 or later
- pyenv (install via Homebrew: `brew install pyenv`)
- Python 3.13.3 (install via pyenv: `pyenv install 3.13.3`)
- pip (included with Python)
- virtualenv (install via pip: `pip install virtualenv`)
- Git (install via Homebrew: `brew install git`)

### Development Setup

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests: `python -m pytest tests/`
5. Submit a pull request

### Testing

```bash
# Run all tests
python -m pytest tests/

# Run specific test file
python -m pytest tests/test_strategy.py

# Run with coverage
python -m pytest --cov=src tests/
```

## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Security

Please report any security issues to security@intent-trader.com

## Support

- Documentation: [docs/](docs/)
- Issues: [GitHub Issues](https://github.com/yourusername/intent-trader/issues)
- Email: support@intent-trader.com

## Acknowledgments

- Thanks to all contributors
- Inspired by various open-source trading systems
- Built with modern Python best practices