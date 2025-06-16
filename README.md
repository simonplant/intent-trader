# Intent Trader - AI-Native Trading Assistant

**Version:** 0.4.3  
**Date:** 2024-03-19  
**Author:** Simon Plant  
**License:** MIT

[![Python 3.6+](https://img.shields.io/badge/python-3.6+-blue.svg)](https://www.python.org/downloads/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Test Coverage](https://img.shields.io/badge/coverage-100%25-brightgreen.svg)](test_intent_trader.py)

An AI-native trading assistant that enforces the complete PFEMRC (Plan-Focus-Execute-Manage-Review-Coach) framework for systematic trading. Runs entirely within AI chat applications with persistent memory across messages. No installation needed - just start chatting!

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

**Intent Trader is a PFEMRC-enforcing trading assistant** that:
- **PLAN**: Scores trades using YOUR systems (DP/Mancini)
- **FOCUS**: Knows when markets are open (futures vs stocks)
- **EXECUTE**: Manages trade statuses automatically
- **MANAGE**: Tracks positions and P&L in real-time
- **REVIEW**: Analyzes chart patterns and momentum
- **COACH**: Tracks behavioral patterns and moderates activity

**This is NOT a Python app you install** - it runs inside your AI of choice!

## Overview

Intent Trader implements the complete PFEMRC framework:

1. **PLAN Phase**
   - Source-based scoring (DP/Mancini)
   - Market hours awareness
   - Trade idea classification
   - Pattern recognition
   - Moderator tracking

2. **FOCUS Phase**
   - Market-open trade filtering
   - Status management
   - Price level tracking
   - Entry validation
   - Risk assessment

3. **EXECUTE Phase**
   - Position sizing by conviction
   - Entry execution
   - Stop placement
   - Target setting
   - Source validation

4. **MANAGE Phase**
   - Real-time P&L tracking
   - Stop management
   - Profit locking
   - Position scaling
   - Risk control

5. **REVIEW Phase**
   - Trade analysis
   - Performance tracking
   - Pattern recognition
   - Behavioral analysis
   - Journal management

6. **COACH Phase**
   - Behavioral pattern detection
   - Overtrading prevention
   - Discipline enforcement
   - Performance coaching
   - Plan alignment scoring

## 🚀 Quick Start (10 seconds)

### The Magic Words:
Just tell your AI the following:

`You are Intent Trader. Initialize and show status.`

or,

`You are Intent Trader, a trading assistant using the Plan-Focus-Execute-Manage-Review-Coach framework. Use the logic and personality defined in the uploaded Python file to help with trading analysis and decisions.`

### What You'll See:
```
=== INTENT TRADER v0.4.3 INITIALIZED ===
Phase: PLAN
Mode: Mode2
Positions: 0
P&L: $0.00

Ready! Try these:
• "analyze dp AAPL focus trade above 225"
• "actionable" - Show trades where market is open
• "mark AAPL triggered" - Update trade status
• "positions" - View current positions
• "help" - See all commands
```

## Installation

### For Users
No installation required! Intent Trader runs entirely within your AI chat application.

### For Developers
```bash
# Clone the repository
git clone https://github.com/simonplant/intent-trader.git
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
├── intent_trader.py      # Main application (75KB)
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

### Market Awareness
- Futures market hours (3:00 AM - 4:00 PM EST)
- Stock market hours (9:30 AM - 4:00 PM EST)
- Automatic status updates
- Market-open trade filtering
- Visual status indicators (🟢/🔴)

### Trade Management
- Trade idea classification system
- Automatic status transitions
- Market-aware trade filtering
- Manual status control
- Enhanced plan display
- Status-based filtering

### Chart Analysis
- Pattern recognition (bull/bear flags, triangles, etc.)
- Traffic light momentum system (🟢/🟡/🔴)
- Automatic trade idea generation
- Pattern-based conviction scoring
- Visual bias indicators

### Moderator Integration
- Real-time moderator trade logging
- Activity tracking and analysis
- Plan alignment scoring
- Trade correlation tracking
- Performance comparison

### Latest Additions (v0.4.3)
- Chart analysis integration
- Color schema detection
- Pattern scoring system
- Traffic light momentum system
- Daily reporting
- Moderator activity tracking
- Plan alignment scoring
- Global price updates
- ES-SPX offset utilities (`show offset`, `set offset <n>|auto`)
- Tomorrow's trade-idea extraction from bull/bear/summary sections
- Manual offset override & auto-decay algorithm
- Enhanced Failed Breakdown & Level Reclaim detection logic
- Real-time overtrading after-stop behavioral alerts

## Architecture

### Technical Stack
- **Language:** Python 3.6+
- **Dependencies:** Zero (stdlib only)
- **Response Time:** <1ms
- **Memory Usage:** ~15MB baseline
- **File Size:** Single 75KB Python file

### Key Components
1. **Intent-Aware Assistant (IAA)**
   - Stateless architecture
   - Pure in-memory operations
   - Natural language processing
   - Context management
   - Market hours awareness
   - Trade status management
   - Pattern recognition
   - Moderator tracking

2. **Data Models**
   ```python
   @dataclass
   class TradeIdea:
       ticker: str
       source: str  # "dp" or "mancini"
       score: ConvictionScore
       status: TradeStatus  # waiting, triggered, invalidated, etc.
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
   - Market Status Classification
   - Trade Status Management
   - Moderator Activity Scoring

### Key Commands
```
# Market Awareness
"actionable" - Show only trades where market is open
"auto update" - Update all statuses based on market conditions

# Status Management
"mark AAPL triggered" - Manually update trade status
"update status AAPL invalidated" - Alternative status update

# Trade Management
"analyze dp AAPL focus trade above 225" - Add trade idea
"execute plan AAPL" - Execute when price hits entry
"positions" - View current positions
"exit AAPL" - Close position

# Chart Analysis
"chart AAPL bull flag above 225" - Analyze chart pattern
"chart ES above 8 and 21" - Check momentum

# Moderator Tracking
"log mod DP bought AAPL 225" - Log moderator trade
```

### Status Management
- **Automatic Updates**
  - Market open/close detection
  - Price-based triggering
  - Stale setup invalidation
  - Status transition tracking

- **Manual Control**
  - Status update commands
  - Market-aware filtering
  - Visual status indicators
  - Enhanced plan display

- **Market Integration**
  - Futures vs stocks awareness
  - Time-based status updates
  - Market-open trade focus
  - Status-based filtering

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