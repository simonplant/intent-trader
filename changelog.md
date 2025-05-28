# Changelog

All notable changes to Intent Trader will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2024-05-28

### Added
- Initial production release
- Complete PFEMRC workflow implementation (Plan, Focus, Execute, Manage, Review, Coach)
- Source-based scoring system for DP/Inner Circle (conviction 0.0-1.0) and Mancini Blueprint (technical 0.0-1.0)
- Real-time position tracking with P&L calculations
- Behavioral pattern detection (revenge trading, overtrading, low conviction)
- 75% profit lock rule for Mancini trades
- SPX source disambiguation to prevent methodology mixing
- Journal system with timestamped entries
- Context save/load functionality for session persistence
- Comprehensive help system
- 40+ unit and integration tests
- Complete documentation with examples

### Technical Details
- Built on Intent-Aware Assistant (IAA) architecture
- Zero dependencies (Python 3.6+ stdlib only)
- <1ms response time for all operations
- Single file deployment (~1000 lines)
- 100% test coverage of critical paths

### Files
- `intent_trader.py` - Main application
- `README.md` - Complete documentation
- `test_intent_trader.py` - Test suite
- `TODO-BACKLOG.md` - Project tracking
- `VERSION` - Version tracking
- `CHANGELOG.md` - This file

## Future Releases

### [1.1.0] - Planned
- Market data integration
- Options Greeks tracking
- Daily report generation

### [1.2.0] - Planned
- Portfolio correlation analysis
- Advanced risk management
- Kelly criterion position sizing