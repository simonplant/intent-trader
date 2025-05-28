# Intent Trader Installation Guide

This guide provides detailed installation instructions for Intent Trader on different platforms.

## Table of Contents

- [System Requirements](#system-requirements)
- [Quick Installation](#quick-installation)
- [Detailed Installation](#detailed-installation)
- [Platform-Specific Instructions](#platform-specific-instructions)
- [Development Installation](#development-installation)
- [Docker Installation](#docker-installation)
- [Verification](#verification)
- [Troubleshooting](#troubleshooting)

## System Requirements

### Minimum Requirements
- **Python**: 3.12 or higher
- **Memory**: 4GB RAM
- **Storage**: 1GB free disk space
- **Operating System**: 
  - macOS 10.15 (Catalina) or later
  - Ubuntu 20.04 LTS or later
  - Windows 10 or later

### Recommended Requirements
- **Python**: 3.12.10 (managed via pyenv)
- **Memory**: 8GB RAM or more
- **Storage**: 5GB free disk space (for market data cache)
- **Operating System**: Latest stable versions

### Dependencies
- Git (for cloning the repository)
- pyenv (for Python version management)
- Make (for build automation)

## Quick Installation

For users who want to get started quickly:

```bash
# Clone the repository
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader

# Set up Python environment
pyenv install 3.12.10
pyenv local 3.12.10

# Create and activate virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Initialize the system
python -c "from src.core.database import DatabaseManager; DatabaseManager().init_database()"

# Verify installation
python -m pytest -v
```

## Detailed Installation

### Step 1: Install Prerequisites

#### macOS
```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install pyenv and Git
brew install pyenv git make

# Add pyenv to shell profile
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.zshrc
echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.zshrc
echo 'eval "$(pyenv init -)"' >> ~/.zshrc

# Reload shell
source ~/.zshrc
```

#### Ubuntu/Debian
```bash
# Update package list
sudo apt update

# Install dependencies
sudo apt install -y make build-essential libssl-dev zlib1g-dev \
libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm \
libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev \
libffi-dev liblzma-dev git

# Install pyenv
curl https://pyenv.run | bash

# Add pyenv to shell profile
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(pyenv init -)"' >> ~/.bashrc

# Reload shell
source ~/.bashrc
```

#### Windows
```powershell
# Install Git for Windows
# Download from: https://git-scm.com/download/win

# Install Python 3.12.10
# Download from: https://www.python.org/downloads/windows/

# Or use Windows Subsystem for Linux (WSL) and follow Ubuntu instructions
```

### Step 2: Clone Repository

```bash
# Clone the repository
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader

# Verify repository structure
ls -la
```

### Step 3: Set Up Python Environment

```bash
# Install Python 3.12.10
pyenv install 3.12.10

# Set local Python version for this project
pyenv local 3.12.10

# Verify Python version
python --version  # Should output: Python 3.12.10
```

### Step 4: Create Virtual Environment

```bash
# Create virtual environment
python -m venv venv

# Activate virtual environment
source venv/bin/activate  # On macOS/Linux
# OR
venv\Scripts\activate     # On Windows

# Verify virtual environment
which python  # Should point to venv/bin/python
```

### Step 5: Install Dependencies

```bash
# Upgrade pip
pip install --upgrade pip

# Install core dependencies
pip install -r requirements.txt

# Verify installation
pip list
```

### Step 6: Initialize System

```bash
# Create necessary directories
mkdir -p data/db data/cache logs

# Initialize database
python -c "from src.core.database import DatabaseManager; DatabaseManager().init_database()"

# Verify database creation
ls -la data/db/
```

### Step 7: Configuration

```bash
# Copy example configuration (if available)
# cp config/config.example.yaml config/config.yaml

# Edit configuration as needed
# nano config/config.yaml
```

## Platform-Specific Instructions

### macOS with Apple Silicon (M1/M2)

```bash
# Install Rosetta 2 (if needed)
softwareupdate --install-rosetta

# Use x86_64 architecture for compatibility (if needed)
arch -x86_64 brew install pyenv

# Install Python with optimizations
PYTHON_CONFIGURE_OPTS="--enable-optimizations" pyenv install 3.12.10
```

### Ubuntu 22.04 LTS

```bash
# Additional dependencies for Ubuntu 22.04
sudo apt install -y python3.12-dev python3.12-venv

# If using system Python
sudo apt install -y python3-pip python3-venv
python3.12 -m venv venv
```

### Windows with WSL2

```bash
# Install WSL2 Ubuntu
wsl --install -d Ubuntu

# Follow Ubuntu installation instructions within WSL
# Access Windows files from WSL: /mnt/c/Users/YourUsername/
```

## Development Installation

For developers who want to contribute to Intent Trader:

```bash
# Follow standard installation steps, then:

# Install development dependencies
pip install -r dev-requirements.txt

# Install pre-commit hooks
pre-commit install

# Run tests to verify development setup
make test

# Run code quality checks
make lint
make type-check
```

### Development Tools Setup

```bash
# Install additional development tools
pip install black isort flake8 mypy pytest-cov

# Set up IDE (VS Code example)
code .

# Install recommended VS Code extensions:
# - Python
# - Pylance
# - Black Formatter
# - GitLens
```

## Docker Installation

For containerized deployment:

### Using Docker Compose

```bash
# Clone repository
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader

# Build and run with Docker Compose
docker-compose up -d

# Verify containers are running
docker-compose ps
```

### Using Dockerfile

```bash
# Build Docker image
docker build -t intent-trader .

# Run container
docker run -d \
  --name intent-trader \
  -p 8000:8000 \
  -v $(pwd)/data:/app/data \
  -v $(pwd)/config:/app/config \
  intent-trader

# Check container status
docker ps
```

### Docker Environment Variables

```bash
# Set environment variables for Docker
docker run -d \
  --name intent-trader \
  -e TRADING_DEFAULT_SYMBOL=AAPL \
  -e LOGGING_LEVEL=INFO \
  -e DATABASE_URL=sqlite:///data/trading.db \
  intent-trader
```

## Verification

### Basic Verification

```bash
# Activate virtual environment
source venv/bin/activate

# Test Python imports
python -c "import src.core.config; print('Core imports successful')"
python -c "import agents.plan_agent; print('Agent imports successful')"

# Run basic functionality test
python scripts/run_first_test.py
```

### Comprehensive Testing

```bash
# Run full test suite
python -m pytest -v

# Run with coverage
python -m pytest --cov=src --cov-report=html

# Check test results
echo "Tests completed. Check htmlcov/index.html for coverage report."
```

### Performance Verification

```bash
# Test market data functionality
python -c "
from src.market_data.feed import MarketDataFeed
feed = MarketDataFeed()
data = feed.get_historical_data('AAPL', period='5d', interval='1d')
print(f'Retrieved {len(data)} data points')
"

# Test strategy engine
python -c "
from src.strategy.engine import StrategyEngine
engine = StrategyEngine()
print('Strategy engine initialized successfully')
"
```

## Troubleshooting

### Common Issues

#### Python Version Issues
```bash
# Check Python version
python --version

# If wrong version, ensure pyenv is properly configured
pyenv versions
pyenv local 3.12.10
```

#### Virtual Environment Issues
```bash
# Deactivate and recreate virtual environment
deactivate
rm -rf venv
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

#### Import Errors
```bash
# Check Python path
python -c "import sys; print('\n'.join(sys.path))"

# Ensure src directory is in path
export PYTHONPATH="${PYTHONPATH}:$(pwd)/src"
```

#### Database Issues
```bash
# Remove and recreate database
rm -f data/db/trading.db
python -c "from src.core.database import DatabaseManager; DatabaseManager().init_database()"
```

#### Permission Issues (Linux/macOS)
```bash
# Fix permissions
chmod +x scripts/*.py
sudo chown -R $USER:$USER .
```

### Platform-Specific Issues

#### macOS: Command Line Tools
```bash
# Install Xcode command line tools
xcode-select --install
```

#### Ubuntu: Missing Dependencies
```bash
# Install missing system dependencies
sudo apt update
sudo apt install -y build-essential libffi-dev libssl-dev
```

#### Windows: Long Path Issues
```powershell
# Enable long paths in Windows
# Run as Administrator:
New-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" -Name "LongPathsEnabled" -Value 1 -PropertyType DWORD -Force
```

### Getting Help

If you encounter issues not covered here:

1. **Check the logs**: Look in `logs/trading.log` for error messages
2. **Search existing issues**: Check [GitHub Issues](https://github.com/yourusername/intent-trader/issues)
3. **Create a new issue**: Include your OS, Python version, and error messages
4. **Join discussions**: Ask questions in [GitHub Discussions](https://github.com/yourusername/intent-trader/discussions)

### Diagnostic Script

Run this script to collect system information for troubleshooting:

```bash
# Create diagnostic script
cat > diagnostic.py << 'EOF'
#!/usr/bin/env python3
import sys
import platform
import subprocess
import os

print("=== Intent Trader Diagnostic Information ===")
print(f"Python version: {sys.version}")
print(f"Platform: {platform.platform()}")
print(f"Architecture: {platform.architecture()}")
print(f"Current directory: {os.getcwd()}")
print(f"Python executable: {sys.executable}")

try:
    result = subprocess.run(['git', '--version'], capture_output=True, text=True)
    print(f"Git version: {result.stdout.strip()}")
except:
    print("Git: Not available")

try:
    import pip
    print(f"Pip version: {pip.__version__}")
except:
    print("Pip: Not available")

print("\n=== Python Path ===")
for path in sys.path:
    print(f"  {path}")

print("\n=== Environment Variables ===")
for key in ['PYTHONPATH', 'PATH', 'PYENV_ROOT']:
    value = os.environ.get(key, 'Not set')
    print(f"{key}: {value}")
EOF

python diagnostic.py
```

## Next Steps

After successful installation:

1. **Read the documentation**: Start with [Getting Started Guide](docs/user-guides/getting-started.md)
2. **Explore examples**: Check out the example scripts in `scripts/`
3. **Run your first trade**: Follow the tutorial in the documentation
4. **Join the community**: Participate in discussions and contribute

---

**Congratulations!** You have successfully installed Intent Trader. Happy trading! 🚀 