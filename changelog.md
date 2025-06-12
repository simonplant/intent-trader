# Changelog

All notable changes to Intent Trader will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Chart analysis integration with `chart` command for visual pattern recognition
- Color schema detection for technical indicators (8/21/50/100/200 MA, VWAP, etc.)
- Pattern scoring: Failed Breakdowns, Bull/Bear Flags, Triangles (0.50-0.85 scores)
- Traffic light momentum system based on 8/21 MA relationship
- Daily reporting with `daily report` command showing comprehensive session summary
- Moderator activity tracking with `log mod [NAME] [ACTION] [TICKER] [PRICE]`
- `export day` command creates markdown-formatted daily logs
- Plan alignment scoring to track execution discipline
- `update` command now works globally for quick price updates across all positions

### Changed
- Enhanced `handle_chart` to auto-create trade ideas from detected patterns
- Improved `handle_update` to accept multiple ticker-price pairs in one command
- Behavioral coaching now includes pattern-specific feedback
- Help system expanded with new chart and reporting commands

### Fixed
- Symbol extraction now properly excludes more non-ticker words (TERM, OUTLOOK, etc.)
- Price level extraction handles comma-separated numbers correctly
- Chart pattern detection properly handles multiple pattern keywords

## [0.4.2] - 2024-06-12

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


### [0.4.3] - Planned
- Refactor into Jupyter Notebook?
- Dynamic trade plan with live status tracking
- Enhanced trade idea model with lifecycle management
- Plan-based execution with trigger detection
- Unified trade plan table view- Market data integration
- Options Greeks tracking
- Portfolio correlation analysis
- Advanced risk management
- Kelly criterion position sizing