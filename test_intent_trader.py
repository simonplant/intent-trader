"""
Intent Trader - Test Suite
Version: 1.0.0
Date: 2024-05-28
Author: Simon Plant
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
        self.assertIn("AAPL: 0.90 = \"Exceptional\"", response)
        self.assertEqual(len(self.trader.context.ideas), 1)
        self.assertEqual(self.trader.context.ideas[0].ticker, "AAPL")
        self.assertEqual(self.trader.context.ideas[0].source, "dp")
        self.assertAlmostEqual(self.trader.context.ideas[0].score.score, 0.90, places=2)
    
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
        self.assertIn("ES Levels: 5740.0, 5750.0, 5765.0", response)
        # Should create both ES and SPX ideas
        es_ideas = [i for i in self.trader.context.ideas if i.ticker == "ES"]
        spx_ideas = [i for i in self.trader.context.ideas if i.ticker == "SPX"]
        self.assertGreaterEqual(len(es_ideas), 1)
        self.assertGreaterEqual(len(spx_ideas), 1)
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
        response = self.trader.process("update AAPL 227.5 TSLA 185.2")
        self.assertIn("Updated: AAPL → 227.5, TSLA → 185.2", response)
        # Check P&L updated
        response = self.trader.process("positions")
        self.assertTrue("$+250.00" in response or "$250.00" in response)  # Accept either format
    
    def test_move_stop(self):
        """Test stop loss management."""
        self.trader.process("buy AAPL @ 225")
        response = self.trader.process("move stop AAPL 224")
        self.assertIn("STOP MOVED", response)
        self.assertIn("New Stop: $224.00", response)
        self.assertIn("Current: $225.00", response)
    
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
        self.assertIn("COACH FEEDBACK", response)
    
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
        self.assertIn("SESSION SAVED", response)
        # Extract JSON from response
        import re
        match = re.search(r'```json\n([\s\S]+?)\n```', response)
        self.assertIsNotNone(match)
        json_str = match.group(1)
        # Reset and load
        self.trader = IntentTrader()
        response = self.trader.process(json_str)  # Pass JSON directly
        self.assertTrue("Loaded from" in response or "SESSION RESTORED" in response)
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
        self.trader.process("update AAPL 227")
        self.assertEqual(self.trader.context.phase, "MANAGE")
        # MANAGE
        self.trader.process("move stop AAPL 224")
        self.trader.process("update AAPL 227")
        # EXIT
        self.trader.process("exit AAPL")
        self.assertEqual(self.trader.context.phase, "REVIEW")
        # REVIEW
        response = self.trader.process("review")
        self.assertIn("session", response.lower())
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
        """Test chart handler for bullish MA alignment and strong long bias."""
        response = self.trader.process("chart shows AAPL above 8 and above 21 with bull flag above yh")
        self.assertIn("strong long", response.lower())
        self.assertIn("buy aapl", response.lower())
        # Should auto-create idea
        idea = next((i for i in self.trader.context.ideas if i.ticker == "AAPL"), None)
        self.assertIsNotNone(idea)
        self.assertGreaterEqual(idea.score.score, 0.70)
        self.assertEqual(idea.source, "dp")

    def test_chart_analysis_bearish(self):
        """Test chart handler for bearish MA alignment and strong short bias."""
        response = self.trader.process("chart shows TSLA below 8 and below 21 with bear flag below yl")
        self.assertIn("strong short", response.lower())
        self.assertIn("buy tsla", response.lower())  # Suggests execution
        idea = next((i for i in self.trader.context.ideas if i.ticker == "TSLA"), None)
        self.assertIsNotNone(idea)
        self.assertGreaterEqual(idea.score.score, 0.70)
        self.assertEqual(idea.source, "dp")

    def test_chart_analysis_mancini_fb(self):
        """Test chart handler for Mancini failed breakdown pattern."""
        response = self.trader.process("chart shows ES fb above yh")
        self.assertIn("mancini_fb", response.lower())
        self.assertIn("strong long", response.lower())
        idea = next((i for i in self.trader.context.ideas if i.ticker == "ES"), None)
        self.assertIsNotNone(idea)
        self.assertEqual(idea.source, "mancini")
        self.assertEqual(idea.score.label, "FB")

    def test_chart_analysis_level_interpretation(self):
        """Test chart handler for level relationship analysis."""
        response = self.trader.process("chart shows AAPL between yh and yl")
        self.assertIn("Range bound", response)
        self.assertIn("WAIT", response)

    def test_chart_analysis_no_pattern(self):
        """Test chart handler with no clear pattern or ticker."""
        response = self.trader.process("chart shows nothing special")
        self.assertIn("YELLOW", response)
        self.assertIn("WAIT", response)

    def test_chart_analysis_journal_update(self):
        """Test that chart handler updates journal with context."""
        self.trader.process("chart shows AAPL above 8 and above 21 with bull flag")
        self.assertTrue(len(self.trader.context.journal) > 0)
        last_entry = self.trader.context.journal[-1]
        self.assertIn("chart: aapl", last_entry.lower())
        self.assertIn("green", last_entry.lower())
        self.assertIn("bull_flag", last_entry.lower())

    def test_chart_analysis_multiple_patterns(self):
        """Test chart handler with multiple patterns, only first detected used."""
        response = self.trader.process("chart shows AAPL bull flag and reclaim above yh")
        self.assertIn("BULL_FLAG", response)
        self.assertNotIn("RECLAIM (score: 0.7)", response)  # Only first pattern

    def test_chart_analysis_no_ticker(self):
        """Test chart handler with no ticker mentioned."""
        response = self.trader.process("chart shows bull flag above yh")
        self.assertIn("strong long", response.lower())
        self.assertIn("this", response.lower())  # No ticker fallback

    def test_chart_analysis_edge_case(self):
        """Test chart handler with ambiguous input."""
        response = self.trader.process("chart shows above 8 but below 21")
        self.assertIn("YELLOW", response)
        self.assertIn("WAIT", response)

    def test_chart_color_legend_single(self):
        """Test chart handler outputs correct legend for a single color mention."""
        response = self.trader.process("chart shows blue line above 8 and bull flag")
        self.assertIn("chart color legend", response.lower())
        self.assertIn("blue = 8 ema", response.lower())
        self.assertIn("(0, 122, 255)", response)

    def test_chart_color_legend_multiple(self):
        """Test chart handler outputs correct legend for multiple color mentions."""
        response = self.trader.process("chart shows blue and orange lines above 8 and 21 with bull flag")
        self.assertIn("chart color legend", response.lower())
        self.assertIn("blue = 8 ema", response.lower())
        self.assertIn("orange = 21 ema", response.lower())
        self.assertIn("(0, 122, 255)", response)
        self.assertIn("(255, 149, 0)", response)

    def test_chart_color_legend_edge_case(self):
        """Test chart handler with unknown color does not break legend output."""
        response = self.trader.process("chart shows magenta line above 8 and bull flag")
        self.assertNotIn("magenta =", response.lower())
        self.assertNotIn("chart color legend", response.lower())  # No legend if no known color

    def test_chart_color_legend_and_pattern(self):
        """Test chart handler outputs both legend and pattern analysis."""
        response = self.trader.process("chart shows blue and purple lines with bull flag above yh")
        self.assertIn("chart color legend", response.lower())
        self.assertIn("blue = 8 ema", response.lower())
        self.assertIn("purple = vwap", response.lower())
        self.assertIn("bull_flag", response.lower())
        self.assertIn("strong long", response.lower())


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