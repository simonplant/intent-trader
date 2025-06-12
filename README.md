# Intent Trader - AI-Native Trading Assistant

**Version:** 0.4.2  
**Date:** 2024-06-12  
**Author:** Simon Plant  
**License:** MIT

[![Python 3.6+](https://img.shields.io/badge/python-3.6+-blue.svg)](https://www.python.org/downloads/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Test Coverage](https://img.shields.io/badge/coverage-100%25-brightgreen.svg)](test_intent_trader.py)

An AI-native trading assistant that runs entirely within AI chat applications, providing structured trading workflow with persistent memory across messages. No installation needed - just start chatting!

## 📋 Table of Contents
- [What This Is](#what-this-is)
- [Overview](#overview)
- [Quick Start](#-quick-start)
- [Installation](#installation)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Features](#features)
- [Architecture](#architecture)
- [Testing](#testing)
- [Contributing](#contributing)
- [License](#license)

## What This Is

**Intent Trader turns AI into a systematic trading assistant** that:
- Remembers all your positions and P&L
- Scores trades using YOUR systems (DP/Mancini)
- Tracks behavioral patterns and coaches you
- Maintains context throughout your trading day

**This is NOT a Python app you install** - it runs inside your AI of choice!

## Overview

Intent Trader is a production-ready trading discipline system that:
- **Never mixes trading methodologies** - Each trade scored 100% by its source
- **Enforces the complete PFEMRC workflow** - PLAN → FOCUS → EXECUTE → MANAGE → REVIEW → COACH
- **Provides real-time behavioral coaching** - Detects revenge trading, overtrading, and discipline breaks
- **Maintains complete audit trail** - Every decision logged and recoverable

## 🚀 Quick Start (10 seconds)

### The Magic Words:
Just tell your AI: **"You are Intent Trader. Initialize and show status."**

That's it! The AI becomes your trading assistant.

### What You'll See:
```
=== INTENT TRADER v0.4.2 INITIALIZED ===
Phase: PLAN
Mode: Mode2
Positions: 0
P&L: $0.00

Ready! Try these:
• "analyze dp AAPL focus trade above 225"
• "buy AAPL"
• "positions"
• "help"
```

## Installation

### For Users
No installation required! Intent Trader runs entirely within your AI chat application.

### For Developers
```bash
# Clone the repository
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader

# Create and activate virtual environment (optional but recommended)
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install development dependencies
pip install -r requirements-dev.txt
```

## Development Setup

1. **Environment Setup**
   ```bash
   # Create virtual environment
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   
   # Install dependencies
   pip install -r requirements-dev.txt
   ```

2. **Running Tests**
   ```bash
   python -m pytest test_intent_trader.py -v
   ```

3. **Code Style**
   ```bash
   # Run linter
   flake8 intent_trader.py
   
   # Run formatter
   black intent_trader.py
   ```

## Project Structure

```
intent-trader/
├── intent_trader.py      # Main application
├── test_intent_trader.py # Test suite
├── README.md            # Documentation
├── CHANGELOG.md         # Version history
├── TODO-BACKLOG.md      # Project tracking
├── version.txt          # Version tracking
├── docs/               # Additional documentation
├── logs/              # Application logs
└── wip/               # Work in progress
```

## Features

### Core Features
- Complete PFEMRC workflow implementation
- Source-based scoring system (DP/Mancini)
- Real-time position tracking with P&L
- Behavioral pattern detection
- 75% profit lock rule for Mancini trades
- SPX source disambiguation
- Journal system with timestamps
- Context save/load functionality
- Comprehensive help system

### Latest Additions (v0.4.2)
- Chart analysis integration
- Color schema detection
- Pattern scoring system
- Traffic light momentum system
- Daily reporting
- Moderator activity tracking
- Plan alignment scoring
- Global price updates

## Architecture

### Technical Stack
- **Language:** Python 3.6+
- **Dependencies:** Zero (stdlib only)
- **Response Time:** <1ms
- **Memory Usage:** ~10MB baseline
- **File Size:** Single 50KB Python file

### Key Components
1. **Intent-Aware Assistant (IAA)**
   - Stateless architecture
   - Pure in-memory operations
   - Natural language processing
   - Context management

2. **Data Models**
   ```python
   @dataclass
   class TradeIdea:
       ticker: str
       source: str  # "dp" or "mancini"
       score: ConvictionScore
       # ... additional fields

   @dataclass
   class Position:
       ticker: str
       source: str
       side: str
       qty: int
       entry: float
       current: float
       stop: Optional[float]

   @dataclass
   class TradingContext:
       phase: str
       mode: str
       ideas: List[TradeIdea]
       positions: List[Position]
       # ... additional fields
   ```

3. **Scoring Systems**
   - DP Conviction Scale (0.0-1.0)
   - Mancini Technical Scoring (0.0-1.0)
   - Pattern Recognition
   - Behavioral Analysis

## Testing

### Test Coverage
- 100% coverage of critical paths
- 40+ unit and integration tests
- Continuous integration ready

### Running Tests
```bash
# Run all tests
python -m pytest test_intent_trader.py -v

# Run specific test
python -m pytest test_intent_trader.py::test_specific_function -v
```

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines
- Follow PEP 8 style guide
- Write tests for new features
- Update documentation
- Keep the single-file architecture
- Maintain zero dependencies

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

Built with the IAA philosophy: *"Stateless, bloatless, chat-first, human-native."*