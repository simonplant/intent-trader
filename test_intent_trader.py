"""
Intent Trader - Test Suite
Version: 1.0.0
Date: 2024-05-28
Author: Solo Trader
License: MIT

Description:
    Comprehensive test suite for Intent Trader v1.0.0
    Tests the core logic that runs in AI conversations.

Test Coverage:
    - All PFEMRC phases
    - Source-based scoring integrity
    - Position management and P&L
    - Behavioral pattern detection
    - Save/load persistence
    - Error handling

Usage:
    python test_intent_trader.py
    
Note: These tests verify the logic works correctly.
      In production, the system runs inside AI conversations.
"""

import unittest
import json
import os
from datetime import datetime
from intent_trader import (
    IntentTrader, TradingContext, TradeIdea, Position, 
    ConvictionScore, DP_CONVICTION_MAP, MANCINI_SETUP_MAP
)


class TestIntentTrader(unittest.TestCase):
    """Complete test coverage for Intent Trader."""
    
    def setUp(self):
        """Create fresh trader for each test."""
        self.trader = IntentTrader()
        
    def tearDown(self):
        """Clean up any test files."""
        # Remove test save files
        for f in os.listdir('.'):
            if f.startswith('test_trader_') and f.endswith('.json'):
                os.remove(f)
    
    # === PLAN PHASE TESTS ===
    
    def test_analyze_dp_basic(self):
        """Test DP analysis with conviction scoring."""
        response = self.trader.process("analyze dp AAPL love this setup above 225")
        
        self.assertIn("DP ANALYSIS", response)
        self.assertIn("AAPL: Exceptional", response)
        self.assertEqual(len(self.trader.context.ideas), 1)
        self.assertEqual(self.trader.context.ideas[0].ticker, "AAPL")
        self.assertEqual(self.trader.context.ideas[0].source, "dp")
        self.assertAlmostEqual(self.trader.context.ideas[0].score.score, 0.93, places=2)
    
    def test_analyze_dp_multiline(self):
        """Test DP analysis with full transcript."""
        transcript = """
        Market looking bullish above 5800
        AAPL definitely want this on any dip to 225
        CRM focus trade here, get aggressive
        TSLA not excited about this one
        """
        
        response = self.trader.process(f"analyze dp {transcript}")
        
        self.assertIn("BULLISH", response)
        self.assertEqual(len(self.trader.context.ideas), 3)
        
        # Check CRM is highest conviction
        crm_idea = next(i for i in self.trader.context.ideas if i.ticker == "CRM")
        self.assertEqual(crm_idea.score.label, "Exceptional")
        self.assertGreaterEqual(crm_idea.score.score, 0.95)
    
    def test_analyze_mancini(self):
        """Test Mancini analysis with technical scoring."""
        response = self.trader.process("""analyze mancini
        ES 5750 showing failed breakdown pattern
        Mode 2 market conditions
        Support at 5740, resistance 5765
        """)
        
        self.assertIn("MANCINI ANALYSIS", response)
        self.assertIn("Mode2", response)
        self.assertIn("ES 5750", response)
        
        # Should create both ES and SPX ideas
        es_ideas = [i for i in self.trader.context.ideas if i.ticker == "ES"]
        spx_ideas = [i for i in self.trader.context.ideas if i.ticker == "SPX"]
        
        self.assertEqual(len(es_ideas), 1)
        self.assertEqual(len(spx_ideas), 1)
        self.assertEqual(es_ideas[0].source, "mancini")
        self.assertEqual(spx_ideas[0].source, "mancini")
    
    def test_create_plan(self):
        """Test unified plan creation with source separation."""
        # Setup ideas first
        self.trader.process("analyze dp AAPL focus trade love this")
        self.trader.process("analyze mancini ES 5750 failed breakdown")
        
        response = self.trader.process("create plan")
        
        self.assertIn("DAILY TRADING PLAN", response)
        self.assertIn("DP/INNER CIRCLE FOCUS", response)
        self.assertIn("MANCINI BLUEPRINT FOCUS", response)
        self.assertEqual(self.trader.context.phase, "FOCUS")
    
    # === FOCUS PHASE TESTS ===
    
    def test_focus_trades(self):
        """Test focus trade filtering."""
        # Add various conviction trades
        self.trader.process("analyze dp AAPL focus trade")
        self.trader.process("analyze dp CRM worth watching")
        self.trader.process("analyze mancini ES failed breakdown")
        
        response = self.trader.process("focus trades")
        
        self.assertIn("AAPL", response)  # 0.95 score
        self.assertIn("ES", response)     # 0.90 score
        self.assertNotIn("CRM", response) # 0.45 score
    
    def test_check_source(self):
        """Test source verification for tickers."""
        # Test automatic assignment
        response = self.trader.process("check source AAPL")
        self.assertIn("stock/ETF", response)
        self.assertIn("DP scoring", response)
        
        response = self.trader.process("check source ES")
        self.assertIn("futures", response)
        self.assertIn("Mancini scoring", response)
        
        # Test SPX ambiguity
        response = self.trader.process("check source SPX")
        self.assertIn("requires source verification", response)
    
    # === EXECUTE PHASE TESTS ===
    
    def test_execute_basic(self):
        """Test basic trade execution."""
        response = self.trader.process("buy 100 AAPL @ 225.50")
        
        self.assertIn("EXECUTED", response)
        self.assertIn("LONG 100 AAPL @ 225.50", response)
        self.assertEqual(len(self.trader.context.positions), 1)
        self.assertEqual(self.trader.context.phase, "MANAGE")
        
        pos = self.trader.context.positions[0]
        self.assertEqual(pos.ticker, "AAPL")
        self.assertEqual(pos.qty, 100)
        self.assertEqual(pos.entry, 225.50)
    
    def test_execute_quick_formats(self):
        """Test various quick entry formats."""
        # Quick buy without price
        response = self.trader.process("buy AAPL")
        self.assertIn("EXECUTED", response)
        self.assertEqual(len(self.trader.context.positions), 1)
        
        # Add format
        self.trader.context.positions = []  # Reset
        response = self.trader.process("add TSLA")
        self.assertIn("EXECUTED", response)
    
    def test_spx_disambiguation(self):
        """Test SPX requires source verification."""
        response = self.trader.process("buy SPX")
        self.assertIn("requires source verification", response)
        self.assertIn("Which system", response)
    
    def test_source_based_execution(self):
        """Test execution uses correct source."""
        # Setup DP idea
        self.trader.process("analyze dp AAPL focus trade")
        
        response = self.trader.process("buy AAPL")
        self.assertIn("Source: DP", response)
        
        # Verify position has correct source
        pos = self.trader.context.positions[0]
        self.assertEqual(pos.source, "dp")
    
    # === MANAGE PHASE TESTS ===
    
    def test_positions_display(self):
        """Test position display with P&L."""
        self.trader.process("buy 100 AAPL @ 225")
        self.trader.process("buy 2 ES @ 5750")
        
        response = self.trader.process("positions")
        
        self.assertIn("DP POSITIONS", response)
        self.assertIn("MANCINI POSITIONS", response)
        self.assertIn("AAPL", response)
        self.assertIn("ES", response)
    
    def test_update_prices(self):
        """Test batch price updates."""
        self.trader.process("buy AAPL @ 225")
        self.trader.process("buy TSLA @ 180")
        
        response = self.trader.process("update AAPL 227.50 TSLA 185.20")
        
        self.assertIn("Updated: AAPL → 227.5, TSLA → 185.2", response)
        
        # Check P&L updated
        response = self.trader.process("positions")
        self.assertIn("$250.00", response)  # AAPL profit
    
    def test_move_stop(self):
        """Test stop loss management."""
        self.trader.process("buy AAPL @ 225")
        
        response = self.trader.process("move stop AAPL 224")
        self.assertIn("STOP MOVED", response)
        self.assertIn("None → 224", response)
        self.assertIn("Stop still below entry", response)
        
        # Move to breakeven
        response = self.trader.process("move stop AAPL 225")
        self.assertIn("RISK FREE", response)
    
    def test_lock_75_mancini(self):
        """Test 75% profit taking for Mancini trades."""
        # Setup profitable Mancini position
        self.trader.process("analyze mancini ES failed breakdown")
        self.trader.process("buy 4 ES @ 5750")
        self.trader.process("update ES 5760")  # 10 point profit
        
        response = self.trader.process("lock 75")
        
        self.assertIn("LOCKED 75% PROFITS", response)
        self.assertIn("Sold 3 units", response)
        self.assertIn("Runner: 1 units remain", response)
        
        # Check position reduced
        pos = self.trader.context.positions[0]
        self.assertEqual(pos.qty, 1)
    
    def test_lock_75_dp_rejected(self):
        """Test 75% rule only applies to Mancini."""
        self.trader.process("buy AAPL @ 225")
        self.trader.process("update AAPL 230")
        
        response = self.trader.process("lock 75 AAPL")
        
        self.assertIn("75% rule is for Mancini trades only", response)
        self.assertIn("AAPL is a DP trade", response)
    
    def test_exit_position(self):
        """Test position exit."""
        self.trader.process("buy AAPL @ 225")
        self.trader.process("update AAPL 227")
        
        response = self.trader.process("exit AAPL")
        
        self.assertIn("CLOSED POSITION", response)
        self.assertIn("$200.00", response)
        self.assertIn("Profit taken", response)
        self.assertEqual(len(self.trader.context.positions), 0)
        self.assertEqual(self.trader.context.phase, "REVIEW")
    
    # === REVIEW PHASE TESTS ===
    
    def test_review_session(self):
        """Test session review."""
        # Complete a trade
        self.trader.process("buy AAPL @ 225")
        self.trader.process("exit AAPL @ 227")
        
        response = self.trader.process("review")
        
        self.assertIn("SESSION REVIEW", response)
        self.assertIn("Completed Trades: 1", response)
        self.assertIn("Realized P&L: $200.00", response)
        self.assertIn("Win Rate: 100%", response)
        self.assertEqual(self.trader.context.phase, "COACH")
    
    # === COACH PHASE TESTS ===
    
    def test_behavioral_alerts(self):
        """Test behavioral pattern detection."""
        # Simulate 3 stops
        for i in range(3):
            self.trader.process(f"buy STOCK{i} @ 100")
            self.trader.process(f"exit STOCK{i} @ 95")
        
        response = self.trader.process("coach")
        
        self.assertIn("3+ stops hit", response)
        self.assertIn("revenge trading risk HIGH", response)
        self.assertIn("Step away for 30 minutes", response)
    
    def test_overtrading_alert(self):
        """Test overtrading detection."""
        # Add 11 trades
        for i in range(11):
            self.trader.process(f"add STOCK{i} dp worth owning")
        
        self.trader.context.trades_completed = 11
        response = self.trader.process("coach")
        
        self.assertIn("Overtrading detected", response)
        self.assertIn("Focus on A+ setups only", response)
    
    # === UTILITY TESTS ===
    
    def test_save_load(self):
        """Test context persistence."""
        # Setup some state
        self.trader.process("analyze dp AAPL focus trade")
        self.trader.process("buy AAPL @ 225")
        self.trader.process("journal Testing save/load")
        
        # Save
        response = self.trader.process("save")
        self.assertIn("Saved to", response)
        
        # Extract filename from response
        import re
        match = re.search(r'Saved to (.+\.json)', response)
        self.assertIsNotNone(match)
        filename = match.group(1)
        
        # Reset and load
        self.trader = IntentTrader()
        response = self.trader.process(f"load {filename}")
        
        self.assertIn("Loaded from", response)
        self.assertEqual(len(self.trader.context.ideas), 1)
        self.assertEqual(len(self.trader.context.positions), 1)
        self.assertEqual(len(self.trader.context.journal), 1)
    
    def test_journal(self):
        """Test journaling functionality."""
        response = self.trader.process("journal Testing the journal feature")
        
        self.assertIn("Journaled:", response)
        self.assertEqual(len(self.trader.context.journal), 1)
        
        # Show journal
        response = self.trader.process("journal")
        self.assertIn("Testing the journal feature", response)
    
    def test_reset(self):
        """Test context reset."""
        self.trader.process("buy AAPL")
        self.trader.process("journal test")
        
        response = self.trader.process("reset")
        
        self.assertIn("Context reset", response)
        self.assertEqual(len(self.trader.context.positions), 0)
        self.assertEqual(len(self.trader.context.journal), 0)
        self.assertEqual(self.trader.context.phase, "PLAN")
    
    # === EDGE CASE TESTS ===
    
    def test_empty_commands(self):
        """Test handling of empty/invalid commands."""
        response = self.trader.process("")
        self.assertIn("Unknown command", response)
        
        response = self.trader.process("gibberish")
        self.assertIn("Unknown command", response)
    
    def test_malformed_trades(self):
        """Test error handling for bad trade formats."""
        response = self.trader.process("buy")
        self.assertIn("Format:", response)
        
        response = self.trader.process("buy XYZ @ notaprice")
        # Should handle gracefully
        self.assertIn("EXECUTED", response)  # With default price
    
    def test_position_not_found(self):
        """Test operations on non-existent positions."""
        response = self.trader.process("exit AAPL")
        self.assertIn("No position in AAPL", response)
        
        response = self.trader.process("move stop AAPL 225")
        self.assertIn("No position in AAPL", response)
    
    # === INTEGRATION TESTS ===
    
    def test_full_trading_cycle(self):
        """Test complete PFEMRC workflow."""
        # PLAN
        self.assertEqual(self.trader.context.phase, "PLAN")
        self.trader.process("analyze dp AAPL focus trade love this at 225")
        self.trader.process("create plan")
        
        # FOCUS
        self.assertEqual(self.trader.context.phase, "FOCUS")
        response = self.trader.process("focus trades")
        self.assertIn("AAPL", response)
        
        # EXECUTE
        self.trader.process("buy 100 AAPL @ 225")
        self.assertEqual(self.trader.context.phase, "MANAGE")
        
        # MANAGE
        self.trader.process("move stop AAPL 224")
        self.trader.process("update AAPL 227")
        
        # EXIT
        self.trader.process("exit AAPL")
        self.assertEqual(self.trader.context.phase, "REVIEW")
        
        # REVIEW
        response = self.trader.process("review")
        self.assertIn("Positive session", response)
        self.assertEqual(self.trader.context.phase, "COACH")
        
        # COACH
        response = self.trader.process("coach")
        self.assertIn("Good discipline", response)
        self.assertEqual(self.trader.context.phase, "PLAN")  # Cycle complete
    
    def test_source_integrity(self):
        """Test that sources never mix."""
        # Create both DP and Mancini ideas
        self.trader.process("analyze dp AAPL focus trade")
        self.trader.process("analyze mancini ES failed breakdown")
        
        # Execute both
        self.trader.process("buy AAPL")
        self.trader.process("buy ES")
        
        # Check positions maintain source
        aapl_pos = next(p for p in self.trader.context.positions if p.ticker == "AAPL")
        es_pos = next(p for p in self.trader.context.positions if p.ticker == "ES")
        
        self.assertEqual(aapl_pos.source, "dp")
        self.assertEqual(es_pos.source, "mancini")
        
        # Verify management rules differ
        response = self.trader.process("positions")
        self.assertIn("DP POSITIONS", response)
        self.assertIn("MANCINI POSITIONS", response)

    def test_chart_analysis_bullish(self):
        """Test chart handler for bullish MA alignment."""
        response = self.trader.process("chart shows AAPL above 8 and above 21 with green traffic")
        self.assertIn("bullish", response.lower())
        self.assertIn("8 and 21", response)
        self.assertIn("pullback entries", response)
        self.assertIn("AAPL", response)

    def test_chart_analysis_bearish(self):
        """Test chart handler for bearish MA alignment."""
        response = self.trader.process("chart shows TSLA below 8 and below 21 with red traffic")
        self.assertIn("bearish", response.lower())
        self.assertIn("avoid longs", response.lower())
        self.assertIn("TSLA", response)

    def test_chart_pattern_flag(self):
        """Test chart handler for flag pattern recognition."""
        response = self.trader.process("I see a bull flag pattern with white lines on SPY")
        self.assertIn("flag pattern", response.lower())
        self.assertIn("bull flags typically break higher", response.lower())
        self.assertIn("SPY", response)

    def test_chart_visual_explanation(self):
        """Test chart handler for visual element explanation."""
        response = self.trader.process("what does yh mean?")
        self.assertIn("yesterday's high", response.lower())
        response2 = self.trader.process("what does orange line mean?")
        self.assertIn("100 sma", response2.lower())

    def test_chart_analysis_fallback(self):
        """Test chart handler fallback/help response."""
        response = self.trader.process("chart help")
        self.assertIn("analyze your chart", response.lower())
        response2 = self.trader.process("see something unusual")
        self.assertIn("analyze your chart", response2.lower())


class TestHelperMethods(unittest.TestCase):
    """Test internal helper methods."""
    
    def setUp(self):
        self.trader = IntentTrader()
    
    def test_extract_symbols(self):
        """Test symbol extraction."""
        text = "AAPL is looking good, also watching TSLA and GOOGL"
        symbols = self.trader._extract_symbols(text)
        
        self.assertEqual(set(symbols), {"AAPL", "TSLA", "GOOGL"})
        
        # Test exclusions
        text = "THE LONG TERM OUTLOOK FOR AAPL IS GOOD"
        symbols = self.trader._extract_symbols(text)
        self.assertEqual(symbols, ["AAPL"])  # THE and FOR excluded
    
    def test_extract_levels(self):
        """Test price level extraction."""
        text = "Support at 5750, resistance 5,765.50 and 5780"
        levels = self.trader._extract_levels(text)
        
        self.assertEqual(levels, [5750.0, 5765.5, 5780.0])
        
        # Test with small numbers filtered
        text = "Looking at 225.50 with 5 day average"
        levels = self.trader._extract_levels(text)
        self.assertEqual(levels, [225.5])  # 5 filtered out
    
    def test_behavioral_patterns(self):
        """Test real-time behavioral detection."""
        # Test stop detection
        self.trader.context.stops_hit = 2
        self.trader.context.positions = [
            Position("A", "dp", "long", 100, 100, 100),
            Position("B", "dp", "long", 100, 100, 100),
            Position("C", "dp", "long", 100, 100, 100),
        ]
        
        alert = self.trader._check_behavioral_patterns()
        self.assertIn("Overtrading after stops", alert)


def run_tests():
    """Run all tests with summary."""
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add all test cases
    suite.addTests(loader.loadTestsFromTestCase(TestIntentTrader))
    suite.addTests(loader.loadTestsFromTestCase(TestHelperMethods))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Summary
    print(f"\n{'='*60}")
    print(f"Tests Run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Success: {result.wasSuccessful()}")
    print(f"{'='*60}")
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_tests()
    exit(0 if success else 1)