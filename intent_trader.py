"""
Intent Trader - AI Trading Assistant (Enhanced)
Version: 1.1.0
Date: 2024-05-28
Author: Simon Plant
License: MIT

Enhanced with trade status tracking and improved plan management.
"""

import re
import json
from datetime import datetime
from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional, Tuple, Any
from enum import Enum

# === CONSTANTS ===

# Position sizing defaults
DEFAULT_POSITION_SIZE = 100
FOCUS_TRADE_SIZE = 100
HIGH_CONVICTION_SIZE = 100
MEDIUM_CONVICTION_SIZE = 50
LOW_CONVICTION_SIZE = 25

# Score thresholds
DP_FOCUS_THRESHOLD = 0.90
DP_HIGH_THRESHOLD = 0.70
DP_MEDIUM_THRESHOLD = 0.50
MANCINI_PRIMARY_THRESHOLD = 0.85
MANCINI_STRONG_THRESHOLD = 0.70

# Risk management
MAX_STOPS_ALLOWED = 3
OVERTRADING_THRESHOLD = 10
LOCK_PROFIT_PERCENTAGE = 0.75
PRICE_ALERT_THRESHOLD = 0.01  # 1% for at-entry alerts

# Limits
MAX_CLOSED_POSITIONS = 100
MAX_JOURNAL_ENTRIES = 500
MAX_MODERATOR_TRADES = 100

# === DATA MODELS ===

@dataclass
class ConvictionScore:
    """Represents a conviction score from a specific source."""
    score: float  # 0.0 to 1.0
    source: str   # "dp" or "mancini"
    label: str    # "Exceptional", "High", "FB", etc.

class TradeStatus(Enum):
    WAITING = "waiting"
    TRIGGERED = "triggered"
    PARTIAL = "partial"
    CLOSED = "closed"
    STOPPED = "stopped"
    INVALIDATED = "invalidated"
    MISSED = "missed"

@dataclass
class TradeIdea:
    """A single trade idea with source-specific scoring."""
    ticker: str
    source: str  # CRITICAL: determines scoring methodology
    score: ConvictionScore
    entry: Optional[float] = None
    stop: Optional[float] = None
    target1: Optional[float] = None
    target2: Optional[float] = None
    size: Optional[str] = None
    notes: Optional[str] = None
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())
    rank: int = 0
    type: str = "long"
    _status: Optional[TradeStatus] = field(default=None, repr=False)
    triggered_at: Optional[float] = None
    current_price: Optional[float] = None

    @property
    def status(self) -> TradeStatus:
        return self._status if self._status is not None else TradeStatus.WAITING

    @status.setter
    def status(self, value: TradeStatus):
        self._status = value

    @property
    def risk_per_share(self) -> float:
        if self.entry and self.stop:
            return abs(self.entry - self.stop)
        return 0

    @property
    def profit_potential(self) -> float:
        if self.entry and self.target2:
            return abs(self.target2 - self.entry)
        return 0

    @property
    def risk_reward(self) -> float:
        if self.risk_per_share > 0:
            return self.profit_potential / self.risk_per_share
        return 0

@dataclass
class Position:
    """Active position tracking."""
    ticker: str
    source: str
    side: str  # "long" or "short"
    qty: int
    entry: float
    current: float
    stop: Optional[float] = None
    
    @property
    def pnl(self) -> float:
        if self.side == "long":
            return (self.current - self.entry) * self.qty
        else:
            return (self.entry - self.current) * self.qty
    
    @property
    def pnl_pct(self) -> float:
        return (self.pnl / (self.entry * self.qty)) * 100 if self.entry > 0 else 0

@dataclass
class TradingContext:
    """Complete trading context - can be serialized/restored."""
    phase: str = "PLAN"
    mode: str = "Mode2"  # Market mode
    ideas: List[TradeIdea] = field(default_factory=list)
    positions: List[Position] = field(default_factory=list)
    realized_pnl: float = 0.0
    stops_hit: int = 0
    trades_completed: int = 0
    journal: List[str] = field(default_factory=list)
    
    # Analysis storage
    dp_analysis: Dict[str, Any] = field(default_factory=dict)
    mancini_analysis: Dict[str, Any] = field(default_factory=dict)
    
    # NEW FIELDS for enhanced reporting
    moderator_trades: List[Dict[str, Any]] = field(default_factory=list)
    closed_positions: List[Position] = field(default_factory=list)
    plan_alignment_score: int = 0

# === SCORING MAPS ===

DP_CONVICTION_MAP = [
    ("focus trade", 0.95, "Exceptional"),
    ("absolute best", 0.95, "Exceptional"),
    ("get aggressive", 0.93, "Exceptional"),
    ("must own", 0.93, "Exceptional"),
    ("no brainer long", 0.93, "Exceptional"),
    ("biggest alpha trade", 0.93, "Exceptional"),
    ("focus today", 0.92, "Exceptional"),
    ("love this", 0.90, "Exceptional"),
    ("favorite", 0.90, "Exceptional"),
    ("favorites", 0.90, "Exceptional"),
    ("love it here", 0.90, "Exceptional"),
    ("A+ setup", 0.90, "Exceptional"),
    ("back up the truck", 0.88, "Exceptional"),
    ("main idea", 0.88, "Exceptional"),
    ("can be a monster", 0.88, "Exceptional"),
    ("fill your bucket", 0.88, "Exceptional"),
    ("big idea", 0.88, "Exceptional"),
    ("core idea", 0.88, "Exceptional"),
    ("focus on", 0.85, "Exceptional"),
    ("definitely want", 0.80, "High"),
    ("buy with both hands", 0.80, "High"),
    ("really like", 0.75, "High"),
    ("add to your position", 0.75, "High"),
    ("add to my exposure", 0.75, "High"),
    ("playing from ahead", 0.75, "High"),
    ("high conviction", 0.75, "High"),
    ("strong conviction", 0.73, "High"),
    ("add back to the position", 0.73, "High"),
    ("buy on dips", 0.73, "High"),
    ("be a buyer of pullbacks", 0.73, "High"),
    ("i'm a fan", 0.70, "High"),
    ("really happy with it", 0.70, "High"),
    ("definitely a buyer", 0.68, "High"),
    ("i'll continue to buy", 0.65, "Medium"),
    ("i'm a buyer", 0.60, "Medium"),
    ("decent setup", 0.55, "Medium"),
    ("medium conviction", 0.55, "Medium"),
    ("worth owning", 0.53, "Medium"),
    ("like it", 0.50, "Medium"),
    ("starter position", 0.50, "Medium"),
    ("would be a great buy", 0.48, "Medium"),
    ("if viable", 0.40, "Low"),
    ("worth watching", 0.38, "Low"),
    ("might work", 0.35, "Low"),
    ("on my radar", 0.30, "Low"),
    ("trim profits", 0.30, "Low"),
    ("take some profits", 0.28, "Low"),
    ("play it by ear", 0.25, "Low"),
    ("wait for better", 0.20, "Avoid"),
    ("not excited", 0.15, "Avoid"),
    ("pass", 0.15, "Avoid"),
    ("avoid", 0.10, "Avoid"),
    ("sell rounds", 0.10, "Avoid"),
    ("take off risk", 0.08, "Avoid"),
    ("stay away", 0.05, "Avoid"),
    ("cut losses", 0.05, "Avoid"),
    ("no way", 0.02, "Avoid"),
]

MANCINI_SETUP_MAP = {
    "failed breakdown": (0.85, "FB"),
    "fb": (0.85, "FB"),
    "failed break": (0.80, "FB"),
    "failed breakout": (0.75, "FBO"),
    "level reclaim": (0.70, "Reclaim"),
    "reclaimed": (0.68, "Reclaim"),
    "support test": (0.55, "Support"),
    "testing support": (0.53, "Support"),
    "of interest": (0.50, "Interest"),
    "mode 2 range": (0.40, "Trap"),
    "range trade": (0.35, "Trap"),
    "against mode": (0.20, "Avoid"),
    "fighting trend": (0.15, "Avoid"),
}

# === MAIN INTENT TRADER ===

class IntentTrader:
    """Complete PFEMRC trading assistant with source-based scoring."""
    
    def __init__(self):
        self.context = TradingContext()
        self.handlers = self._register_handlers()
        
    def __str__(self):
        """Display startup screen."""
        return f"""
=== INTENT TRADER v1.1.0 ===
Phase: {self.context.phase}
Mode: {self.context.mode}
Positions: {len(self.context.positions)}
P&L: ${self.context.realized_pnl:.2f}

Quick Commands:
• analyze dp [morning call]
• buy AAPL
• positions
• show plan
• help

What's your first move?
"""
    
    def _register_handlers(self) -> Dict[str, callable]:
        """Register all intent handlers."""
        return {
            # PLAN Phase
            "analyze dp": self.handle_analyze_dp,
            "analyze mancini": self.handle_analyze_mancini,
            "market mode": self.handle_market_mode,
            "create plan": self.handle_create_plan,
            
            # FOCUS Phase
            "focus trades": self.handle_focus_trades,
            "dp focus": self.handle_dp_focus,
            "mancini setups": self.handle_mancini_setups,
            "check source": self.handle_check_source,
            
            # EXECUTE Phase
            "add": self.handle_add_trade,
            "buy": self.handle_execute,
            "sell": self.handle_execute,
            "size": self.handle_size_position,
            
            # MANAGE Phase
            "positions": self.handle_positions,
            "lock 75": self.handle_lock_profits,
            "move stop": self.handle_move_stop,
            "exit": self.handle_exit,
            
            # REVIEW Phase
            "review": self.handle_review,
            "performance": self.handle_performance,
            "daily report": self.handle_daily_report,
            
            # COACH Phase
            "coach": self.handle_coach,
            "behavioral": self.handle_behavioral_check,
            
            # Utilities
            "help": self.handle_help,
            "save": self.handle_save,
            "load": self.handle_load,
            "journal": self.handle_journal,
            "log mod": self.handle_log_moderator,
            "export day": self.handle_export_day,
            "reset": self.handle_reset,
            "context": self.handle_context,
            "chart": self.handle_chart,
            "see": self.handle_chart,
            "mean": self.handle_chart,
            "update": self.handle_update,
            "quick": self.handle_quick,
            "note": self.handle_note,
            
            # NEW Enhanced Plan Management
            "show plan": self.handle_show_plan,
            "plan table": self.handle_show_plan,
            "update prices": self.handle_update_prices,
            "waiting": self.handle_show_plan,
            "active": self.handle_show_plan,
            "done": self.handle_show_plan,
            "execute plan": self.handle_execute_from_plan,
            "invalidate": self.handle_invalidate,
        }
    
    def process(self, message: str) -> str:
        """Main entry point - process any message."""
        # Robust: If message is a JSON object, treat as load
        if re.match(r'^\s*\{[\s\S]*\}\s*$', message.strip()):
            return self.handle_load(message)
        msg_lower = message.lower()
        
        # Route to handler
        for intent, handler in self.handlers.items():
            if intent in msg_lower:
                response = handler(message)
                
                # Check behavioral patterns
                alert = self._check_behavioral_patterns()
                if alert:
                    response += f"\n\n{alert}"
                    
                return response
                
        return self.handle_unknown(message)
    
    # === PLAN PHASE HANDLERS ===
    
    def handle_analyze_dp(self, message: str) -> str:
        """Analyze DP morning call with enhanced conviction scoring."""
        lines = message.split('\n')
        analysis = {
            "bias": "NEUTRAL",
            "ideas": [],
            "levels": [],
            "conviction_phrases": [],
            "energy_summary": {"HIGH": 0, "MEDIUM": 0, "LOW": 0}
        }
        all_text = message.lower()
        levels = self._extract_levels(message)
        symbols = self._extract_symbols(message)
        # Deduplicate symbols
        seen_symbols = set()
        unique_symbols = []
        for symbol in symbols:
            if symbol not in seen_symbols:
                seen_symbols.add(symbol)
                unique_symbols.append(symbol)
        # Determine bias
        if "bullish" in all_text and any(w in all_text for w in ["above", "over", "break"]):
            analysis["bias"] = "BULLISH"
        elif "bearish" in all_text and any(w in all_text for w in ["below", "under", "fail"]):
            analysis["bias"] = "BEARISH"
        # Enhanced scoring for each line
        dp_analyzer = DPConvictionAnalyzer()
        found_count = 0
        for line in lines:
            symbols_in_line = self._extract_symbols(line)
            for symbol in symbols_in_line:
                result = dp_analyzer.analyze_line(line, symbol)
                if result['score'] is not None:
                    idea = TradeIdea(
                        ticker=symbol,
                        source="dp",
                        score=ConvictionScore(result['score'], "dp", result['label']),
                        notes=f"Energy: {result['energy']} | Confidence: {result['confidence']:.0%}"
                    )
                    # Extract price if mentioned
                    prices = self._extract_levels(line)
                    if prices:
                        idea.entry = prices[0]
                    self.context.ideas.append(idea)
                    analysis["ideas"].append(
                        f"{symbol}: {result['label']} ({result['score']:.2f}) - {result['energy']} energy"
                    )
                    analysis["energy_summary"][result['energy']] += 1
                    if result['score'] >= 0.90:
                        analysis["conviction_phrases"].append(line.strip())
                    found_count += 1
        # Store analysis
        analysis["levels"] = levels
        self.context.dp_analysis = analysis
        self.context.phase = "PLAN"
        # Format response
        response = "=== DP ANALYSIS ===\n"
        response += f"Bias: {analysis['bias']}\n"
        response += f"Key Levels: {', '.join(map(str, levels[:5]))}\n"
        response += f"\nFound {found_count} trade ideas from {len(unique_symbols)} tickers\n"
        response += f"Energy Summary: HIGH={analysis['energy_summary']['HIGH']}, MEDIUM={analysis['energy_summary']['MEDIUM']}, LOW={analysis['energy_summary']['LOW']}\n"
        if self.context.ideas:
            response += "\nCONVICTION SCORING (with energy/confidence):\n"
            # Group by conviction level
            exceptional = [i for i in self.context.ideas if i.source == "dp" and i.score.score >= 0.90]
            high = [i for i in self.context.ideas if i.source == "dp" and 0.70 <= i.score.score < 0.90]
            medium = [i for i in self.context.ideas if i.source == "dp" and 0.50 <= i.score.score < 0.70]
            
            if exceptional:
                response += "\nEXCEPTIONAL (0.90+) - Focus Trades:\n"
                response += "| TICKER | SOURCE | SCORE | STATUS | ENTRY | STOP | T1 | T2 | R:R |\n"
                response += "|--------|--------|-------|--------|-------|------|----|----|-----|\n"
                for i in exceptional:
                    entry_str = f"{i.entry:.2f}" if i.entry else "---"
                    stop_str = f"{i.stop:.2f}" if i.stop else "---"
                    t1_str = f"{i.target1:.2f}" if i.target1 else "---"
                    t2_str = f"{i.target2:.2f}" if i.target2 else "---"
                    rr_str = f"{i.risk_reward:.1f}:1" if i.risk_reward > 0 else "---"
                    response += f"| {i.ticker:<6} | {i.source:<6} | {i.score.score:.2f} | {i.status.value:<7} | {entry_str:<5} | {stop_str:<5} | {t1_str:<5} | {t2_str:<5} | {rr_str:<5} |\n"
                    
            if high:
                response += "\nHIGH (0.70-0.89) - Full Size:\n"
                response += "| TICKER | SOURCE | SCORE | STATUS | ENTRY | STOP | T1 | T2 | R:R |\n"
                response += "|--------|--------|-------|--------|-------|------|----|----|-----|\n"
                for i in high:
                    entry_str = f"{i.entry:.2f}" if i.entry else "---"
                    stop_str = f"{i.stop:.2f}" if i.stop else "---"
                    t1_str = f"{i.target1:.2f}" if i.target1 else "---"
                    t2_str = f"{i.target2:.2f}" if i.target2 else "---"
                    rr_str = f"{i.risk_reward:.1f}:1" if i.risk_reward > 0 else "---"
                    response += f"| {i.ticker:<6} | {i.source:<6} | {i.score.score:.2f} | {i.status.value:<7} | {entry_str:<5} | {stop_str:<5} | {t1_str:<5} | {t2_str:<5} | {rr_str:<5} |\n"
                    
            if medium:
                response += "\nMEDIUM (0.50-0.69) - Half Size:\n"
                response += "| TICKER | SOURCE | SCORE | STATUS | ENTRY | STOP | T1 | T2 | R:R |\n"
                response += "|--------|--------|-------|--------|-------|------|----|----|-----|\n"
                for i in medium:
                    entry_str = f"{i.entry:.2f}" if i.entry else "---"
                    stop_str = f"{i.stop:.2f}" if i.stop else "---"
                    t1_str = f"{i.target1:.2f}" if i.target1 else "---"
                    t2_str = f"{i.target2:.2f}" if i.target2 else "---"
                    rr_str = f"{i.risk_reward:.1f}:1" if i.risk_reward > 0 else "---"
                    response += f"| {i.ticker:<6} | {i.source:<6} | {i.score.score:.2f} | {i.status.value:<7} | {entry_str:<5} | {stop_str:<5} | {t1_str:<5} | {t2_str:<5} | {rr_str:<5} |\n"
                    
        response += "\n-> Next: 'analyze mancini' or 'create plan'"
        return response
    
    def handle_analyze_mancini(self, message: str) -> str:
        """Analyze Mancini newsletter with technical scoring."""
        analysis = {
            "mode": "Mode2",  # Default
            "setups": [],
            "levels": []
        }
        
        msg_lower = message.lower()
        levels = self._extract_levels(message)
        
        # Detect market mode
        if "trending" in msg_lower or "mode 1" in msg_lower:
            analysis["mode"] = "Mode1"
        elif "complex" in msg_lower or "mode 2" in msg_lower:
            analysis["mode"] = "Mode2"
            
        # Find setups
        found_setups = []
        for pattern, (score, label) in MANCINI_SETUP_MAP.items():
            if pattern in msg_lower:
                # ES is the primary instrument for Mancini
                idea = TradeIdea(
                    ticker="ES",
                    source="mancini",
                    score=ConvictionScore(score, "mancini", label)
                )
                
                # Add SPX equivalent if ES levels found
                if levels and levels[0] > 1000:  # Likely ES level
                    es_level = levels[0]
                    idea.entry = es_level
                    idea.notes = f"ES {es_level} = SPX {es_level/10:.0f}"
                    
                    # Also create SPX idea
                    spx_idea = TradeIdea(
                        ticker="SPX",
                        source="mancini",
                        score=ConvictionScore(score, "mancini", label),
                        entry=es_level/10,
                        notes=f"Derived from ES {es_level}"
                    )
                    
                    self.context.ideas.extend([idea, spx_idea])
                    analysis["setups"].append(f"{label} @ ES {es_level}")
                    found_setups.append((label, score, es_level))
                else:
                    self.context.ideas.append(idea)
                    analysis["setups"].append(f"{label} (no level)")
                    found_setups.append((label, score, None))
                    
        # Store analysis
        analysis["levels"] = levels
        self.context.mancini_analysis = analysis
        self.context.mode = analysis["mode"]
        
        # Format response
        response = "=== MANCINI ANALYSIS ===\n"
        response += f"Market Mode: {analysis['mode']}\n"
        response += f"ES Levels: {', '.join(map(str, levels[:4]))}\n"
        
        if found_setups:
            response += "\nTECHNICAL SCORING:\n"
            for label, score, level in found_setups:
                level_str = f" @ ES {level}" if level else ""
                response += f"  * {label}: {score:.2f}{level_str}\n"
                    
        response += "\n-> Next: 'create plan'"
        return response
    
    def handle_create_plan(self, message: str) -> str:
        """Create trading plan maintaining source separation."""
        dp_ideas = [i for i in self.context.ideas if i.source == "dp"]
        mancini_ideas = [i for i in self.context.ideas if i.source == "mancini"]
        
        response = "=== DAILY TRADING PLAN ===\n"
        response += f"Phase: PLAN -> FOCUS\n"
        response += f"Market Mode: {self.context.mode}\n\n"
        
        # DP Section
        if dp_ideas:
            response += "DP/INNER CIRCLE FOCUS:\n"
            focus_trades = [i for i in dp_ideas if i.score.score >= 0.90]
            high_conviction = [i for i in dp_ideas if 0.70 <= i.score.score < 0.90]
            
            if focus_trades:
                response += "Focus Trades (0.90+):\n"
                for idea in focus_trades[:3]:
                    response += f"  * {idea.ticker}: {idea.score.label} ({idea.score.score:.2f})\n"
                    
            if high_conviction:
                response += "High Conviction (0.70-0.89):\n"
                for idea in high_conviction[:3]:
                    response += f"  * {idea.ticker}: {idea.score.label} ({idea.score.score:.2f})\n"
        
        # Mancini Section
        if mancini_ideas:
            response += "\nMANCINI BLUEPRINT FOCUS:\n"
            fb_setups = [i for i in mancini_ideas if i.score.label == "FB"]
            other_setups = [i for i in mancini_ideas if i.score.label != "FB"]
            
            if fb_setups:
                response += "Failed Breakdowns (Primary Edge):\n"
                for idea in fb_setups:
                    entry_str = f" @ {idea.entry}" if idea.entry else ""
                    response += f"  * {idea.ticker}: {idea.score.label}{entry_str}\n"
                    if idea.notes:
                        response += f"    -> {idea.notes}\n"
                        
            if other_setups:
                response += "Other Setups:\n"
                for idea in other_setups[:2]:
                    response += f"  * {idea.ticker}: {idea.score.label} ({idea.score.score:.2f})\n"
        
        # Execution rules
        response += "\nEXECUTION RULES:\n"
        response += "* DP trades: Size by conviction score\n"
        response += "* Mancini trades: Wait for acceptance confirmation\n"
        response += "* Never mix scoring methodologies\n"
        response += "* Verify source before ANY SPX trade\n"
        
        self.context.phase = "FOCUS"
        response += "\n-> Phase updated to FOCUS"
        response += "\n-> Use 'show plan' to see your live trading table"
        
        return response
    
    # === NEW ENHANCED PLAN MANAGEMENT ===
    
    def handle_show_plan(self, message: str) -> str:
        """Show the current trading plan with optional status filtering."""
        msg_lower = message.lower()
        
        # Determine filter
        status_filter = None
        if "waiting" in msg_lower:
            status_filter = TradeStatus.WAITING
        elif "active" in msg_lower:
            status_filter = TradeStatus.TRIGGERED
        elif "done" in msg_lower:
            status_filter = [TradeStatus.CLOSED, TradeStatus.STOPPED, TradeStatus.INVALIDATED]
        
        response = "=== CURRENT TRADING PLAN ===\n"
        response += f"Phase: {self.context.phase} | Mode: {self.context.mode}\n\n"
        
        # Filter ideas if requested
        ideas_to_show = self.context.ideas
        if status_filter:
            if isinstance(status_filter, list):
                ideas_to_show = [i for i in self.context.ideas if i.status in status_filter]
            else:
                ideas_to_show = [i for i in self.context.ideas if i.status == status_filter]
        
        if not ideas_to_show:
            response += "No trade ideas match this filter.\n"
            return response
        
        # Create table format
        response += "| TICKER | SOURCE | SCORE | STATUS | ENTRY | STOP | T1 | T2 | R:R |\n"
        response += "|--------|--------|-------|--------|-------|------|----|----|-----|\n"
        
        for idea in sorted(ideas_to_show, key=lambda x: (-x.score.score, x.ticker)):
            entry_str = f"{idea.entry:.2f}" if idea.entry else "---"
            stop_str = f"{idea.stop:.2f}" if idea.stop else "---"
            t1_str = f"{idea.target1:.2f}" if idea.target1 else "---"
            t2_str = f"{idea.target2:.2f}" if idea.target2 else "---"
            rr_str = f"{idea.risk_reward:.1f}:1" if idea.risk_reward > 0 else "---"
            
            response += f"| {idea.ticker:<6} | {idea.source:<6} | {idea.score.score:.2f} | {idea.status.value:<10} | {entry_str:<5} | {stop_str:<5} | {t1_str:<5} | {t2_str:<5} | {rr_str:<5} |\n"
        
        # Add action hints
        response += "\nACTIONS:\n"
        response += "• 'update prices AAPL 225 TSLA 420' - Update current prices\n"
        response += "• 'execute plan AAPL' - Execute when price hits entry\n"
        response += "• 'invalidate AAPL' - Remove setup that's no longer valid\n"
        response += "• 'waiting/active/done' - Filter by status\n"
        
        return response
    
    def handle_update_prices(self, message: str) -> str:
        """Update current prices for trade ideas."""
        parts = message.split()[2:]  # Skip 'update prices'
        
        if not parts or len(parts) % 2 != 0:
            return "Format: update prices AAPL 227.50 TSLA 185.20"
        
        updated = []
        for i in range(0, len(parts), 2):
            ticker = parts[i].upper()
            try:
                price = float(parts[i + 1])
                
                # Update all ideas for this ticker
                ideas_updated = 0
                for idea in self.context.ideas:
                    if idea.ticker == ticker:
                        idea.current_price = price
                        ideas_updated += 1
                
                # Also update positions
                for pos in self.context.positions:
                    if pos.ticker == ticker:
                        pos.current = price
                        
                if ideas_updated > 0:
                    updated.append(f"{ticker} → ${price:.2f}")
                    
            except ValueError:
                return f"Invalid price for {ticker}"
        
        if updated:
            response = "Updated prices: " + ", ".join(updated)
            response += "\n\n" + self.handle_show_plan("")
            return response
        
        return "No matching tickers found to update"
    
    def handle_execute_from_plan(self, message: str) -> str:
        """Execute a trade from the plan when it hits entry."""
        parts = message.split()
        if len(parts) < 3:
            return "Format: execute plan AAPL"
            
        ticker = parts[2].upper()
        
        # Find the idea
        idea = next((i for i in self.context.ideas if i.ticker == ticker), None)
        if not idea:
            return f"❌ {ticker} not in trade plan\n\n" + self.handle_show_plan("")
        
        # Validate status
        if idea.status != TradeStatus.WAITING:
            return f"❌ {ticker} status is {idea.status.value}, not waiting\n\n" + self.handle_show_plan("")
        
        # Validate price
        if not idea.current_price:
            return f"❌ No current price for {ticker}. Run 'update prices' first\n\n" + self.handle_show_plan("")
        
        # Check if at entry
        if idea.type == "long" and idea.current_price > idea.entry * 1.01:
            return f"❌ {ticker} above entry ${idea.entry:.2f} (current: ${idea.current_price:.2f})\n\n" + self.handle_show_plan("")
        elif idea.type == "short" and idea.current_price < idea.entry * 0.99:
            return f"❌ {ticker} below entry ${idea.entry:.2f} (current: ${idea.current_price:.2f})\n\n" + self.handle_show_plan("")
        
        # Risk check
        if self.context.stops_hit >= MAX_STOPS_ALLOWED:
            return f"❌ Cannot execute: {MAX_STOPS_ALLOWED} stops already hit. Maximum risk reached.\n\n" + self.handle_show_plan("")
        
        # Calculate position size based on conviction
        if idea.score.score >= DP_FOCUS_THRESHOLD:
            qty = FOCUS_TRADE_SIZE
        elif idea.score.score >= DP_HIGH_THRESHOLD:
            qty = HIGH_CONVICTION_SIZE
        elif idea.score.score >= DP_MEDIUM_THRESHOLD:
            qty = MEDIUM_CONVICTION_SIZE
        else:
            qty = LOW_CONVICTION_SIZE
        
        # Mode adjustment
        if self.context.mode == "Mode2":
            qty = int(qty * 0.75)  # Reduce size in choppy markets
        
        # Create position
        position = Position(
            ticker=ticker,
            source=idea.source,
            side=idea.type,
            qty=qty,
            entry=idea.current_price,
            current=idea.current_price,
            stop=idea.stop
        )
        
        self.context.positions.append(position)
        idea.status = TradeStatus.TRIGGERED
        idea.triggered_at = idea.current_price
        self.context.phase = "MANAGE"
        
        response = f"✅ TRIGGERED {idea.ticker} {idea.type.upper()} {qty} @ ${idea.current_price:.2f}\n"
        response += f"Source: {idea.source.upper()} ({idea.score.label})\n"
        response += f"Stop: ${idea.stop:.2f} | T1: ${idea.target1:.2f} | T2: ${idea.target2:.2f}\n"
        response += f"Risk: ${idea.risk_per_share:.2f}/share | R:R: {idea.risk_reward:.1f}:1\n"
        
        if idea.source == "mancini":
            response += "\n📋 Mancini Protocol:\n• Lock 75% at T1\n• Trail runner to T2\n"
        else:
            response += "\n📋 DP Management:\n• Flexible based on action\n• Consider sentiment shifts\n"
        
        response += "\n" + self.handle_show_plan("")
        return response
    
    def handle_invalidate(self, message: str) -> str:
        """Invalidate a setup that's no longer valid."""
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify ticker: 'invalidate AAPL broke support'"
            
        ticker = symbols[0]
        reason = " ".join(message.split()[2:]) if len(message.split()) > 2 else "manual"
        
        # Find and invalidate all waiting ideas for this ticker
        invalidated = []
        for idea in self.context.ideas:
            if idea.ticker == ticker and idea.status == TradeStatus.WAITING:
                idea.status = TradeStatus.INVALIDATED
                invalidated.append(idea)
        
        if invalidated:
            response = f"❌ Invalidated {ticker}: {reason}\n"
            response += f"Removed {len(invalidated)} setup(s) from active plan\n"
        else:
            response = f"No waiting setups for {ticker} to invalidate\n"
        
        response += "\n" + self.handle_show_plan("")
        return response
    
    # === REMAINING CORE HANDLERS ===
    
    def handle_execute(self, message: str) -> str:
        """Execute a trade with source validation."""
        pattern = r'(?:buy|sell|long|short|add)\s+(?:(\d+)\s+)?([A-Z]+)(?:\s+@\s+|\s+at\s+)?([\d.]+)?'
        match = re.search(pattern, message, re.I)
        
        if not match:
            return "Format: buy 100 AAPL @ 225.50 or buy AAPL"
            
        qty_str, ticker, price_str = match.groups()
        
        action = message.split()[0].lower()
        if action == "add":
            action = "buy"
            
        side = "long" if action in ["buy", "long", "add"] else "short"
        qty = int(qty_str) if qty_str else 100
        price = float(price_str) if price_str else 0.0
        
        # Find source for this ticker
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        if not ideas:
            if ticker == "SPX":
                return "! SPX trade requires source verification. Which system?"
            source = "mancini" if ticker in ["ES", "NQ", "RTY", "YM"] else "dp"
        else:
            best_idea = max(ideas, key=lambda i: i.score.score)
            source = best_idea.source
            
        # Create position
        position = Position(
            ticker=ticker,
            source=source,
            side=side,
            qty=qty,
            entry=price if price > 0 else 100.0,
            current=price if price > 0 else 100.0,
            stop=None
        )
        
        self.context.positions.append(position)
        self.context.phase = "MANAGE"
        
        response = f"✅ EXECUTED: {side.upper()} {qty} {ticker}"
        if price > 0:
            response += f" @ ${price:.2f}"
        response += f"\nSource: {source.upper()}\n"
        
        return response
    
    def handle_positions(self, message: str) -> str:
        """Show current positions with P&L."""
        if not self.context.positions:
            return "No open positions"
            
        response = "=== OPEN POSITIONS ===\n"
        total_unrealized = 0
        
        # Table format
        response += "| TICKER | SOURCE | SIDE | QTY@PRICE | CURRENT | P&L | % |\n"
        response += "|--------|--------|------|-----------|---------|-----|-----|\n"
        
        for pos in self.context.positions:
            response += f"| {pos.ticker:<6} | {pos.source:<6} | {pos.side:<4} | "
            response += f"{pos.qty}@{pos.entry:.2f} | {pos.current:.2f} | "
            response += f"${pos.pnl:+.2f} | {pos.pnl_pct:+.1f}% |\n"
            total_unrealized += pos.pnl
        
        response += f"\nUnrealized: ${total_unrealized:+.2f}\n"
        response += f"Realized: ${self.context.realized_pnl:.2f}\n"
        response += f"Total P&L: ${self.context.realized_pnl + total_unrealized:+.2f}"
        
        return response
    
    def handle_exit(self, message: str) -> str:
        """Exit a position."""
        if "all" in message.lower():
            if not self.context.positions:
                return "No positions to exit"
                
            total_pnl = sum(p.pnl for p in self.context.positions)
            self.context.realized_pnl += total_pnl
            self.context.trades_completed += len(self.context.positions)
            
            stops_hit = sum(1 for p in self.context.positions if p.pnl < 0)
            self.context.stops_hit += stops_hit
            
            self.context.closed_positions.extend(self.context.positions)
            self.context.positions = []
            self.context.phase = "REVIEW"
            
            return f"✅ CLOSED ALL: P&L ${total_pnl:+.2f}\n-> Phase: REVIEW"
            
        # Exit specific position
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify: 'exit AAPL' or 'exit all'"
            
        ticker = symbols[0]
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        
        if not pos:
            return f"No position in {ticker}"
            
        # Update price if provided
        levels = self._extract_levels(message)
        if levels:
            pos.current = levels[0]
            
        self.context.closed_positions.append(pos)
        self.context.positions.remove(pos)
        self.context.realized_pnl += pos.pnl
        self.context.trades_completed += 1
        
        if pos.pnl < 0:
            self.context.stops_hit += 1
            
        response = f"✅ CLOSED {ticker}: ${pos.pnl:+.2f} ({pos.pnl_pct:+.1f}%)\n"
        response += f"Source: {pos.source.upper()}\n"
        
        if pos.pnl < 0:
            response += "❌ Stop hit"
        else:
            response += "✅ Profit taken"
            
        if not self.context.positions:
            self.context.phase = "REVIEW"
            response += "\n\n-> All flat. Phase: REVIEW"
            
        return response
    
    def handle_help(self, message: str) -> str:
        """Show help information."""
        return """
=== INTENT TRADER v1.1.0 HELP ===

WORKFLOW:
1. PLAN: Analyze DP/Mancini → Create plan
2. FOCUS: Review focus trades → Add setups
3. EXECUTE: Update prices → Execute when at entry
4. MANAGE: Monitor positions → Exit at targets/stops
5. REVIEW: Check performance → Get coaching

KEY COMMANDS:
📊 Planning & Analysis:
• analyze dp [morning call text]
• analyze mancini [newsletter text]
• create plan
• show plan / waiting / active / done

💹 Trade Execution:
• add AAPL dp focus trade entry 225 stop 223 target 230 235
• update prices AAPL 225.50 TSLA 420.69
• execute plan AAPL (when price hits entry)
• buy AAPL / sell AAPL

📈 Position Management:
• positions
• move stop AAPL 224
• lock 75 (Mancini trades)
• exit AAPL / exit all

📝 Utilities:
• save / load
• journal [note]
• chart [description]
• coach / review

Say commands naturally - I understand variations!
"""
    
    # === HELPER METHODS ===
    
    def _extract_symbols(self, text: str) -> List[str]:
        """Extract stock symbols from text."""
        symbols = re.findall(r'\b[A-Z]{2,5}\b', text)
        exclude = {'THE', 'AND', 'FOR', 'BUY', 'SELL', 'LONG', 'SHORT', 'AT', 'TO', 'BE', 
                  'IS', 'ON', 'IN', 'WITH', 'TERM', 'OUTLOOK', 'GOOD', 'ALSO', 'WATCHING',
                  'ES', 'NQ', 'RTY', 'YM', 'SPX', 'SPY', 'QQQ', 'IWM', 'DIA'}
        return [s for s in symbols if s not in exclude or s in ['ES', 'NQ', 'RTY', 'YM', 'SPX', 'SPY', 'QQQ', 'IWM', 'DIA']]
    
    def _extract_levels(self, text: str) -> List[float]:
        """Extract price levels from text."""
        text = text.replace(',', '')
        prices = re.findall(r'\b(\d{2,6}(?:\.\d{1,2})?)\b', text)
        
        levels = []
        for p in prices:
            try:
                val = float(p)
                if 10 < val < 99999:
                    levels.append(val)
            except:
                pass
                
        return sorted(set(levels))
    
    def _check_behavioral_patterns(self) -> Optional[str]:
        """Check for behavioral issues in real-time."""
        if self.context.stops_hit >= 2 and len(self.context.positions) > 2:
            return "⚠️ COACH ALERT: Overtrading after stops! Reduce size or step away."
            
        if self.context.stops_hit >= 3:
            return "🛑 COACH ALERT: 3 stops hit - Maximum risk reached. No new trades!"
            
        if self.context.positions:
            active_ideas = []
            for pos in self.context.positions:
                idea = next((i for i in self.context.ideas if i.ticker == pos.ticker), None)
                if idea:
                    active_ideas.append(idea)
                    
            if active_ideas and all(i.score.score < 0.50 for i in active_ideas):
                return "⚠️ COACH ALERT: All positions are low conviction. Raise your standards!"
                
        return None
    
    def handle_add_trade(self, message: str) -> str:
        """Add a new trade idea with proper source routing and full parameters."""
        parts = message.split()
        if len(parts) < 2:
            return "Usage: add AAPL dp love this setup entry 225 stop 223 target 230 235"
            
        ticker = parts[1].upper()
        
        # Parse parameters
        entry = stop = target1 = target2 = None
        msg_lower = message.lower()
        
        # Extract parameters using regex
        entry_match = re.search(r'entry\s+([\d.]+)', msg_lower)
        if entry_match:
            entry = float(entry_match.group(1))
            
        stop_match = re.search(r'stop\s+([\d.]+)', msg_lower)
        if stop_match:
            stop = float(stop_match.group(1))
            
        # Handle multiple target formats
        target_matches = re.findall(r'target\s+([\d.]+)', msg_lower)
        if target_matches:
            target1 = float(target_matches[0])
            if len(target_matches) > 1:
                target2 = float(target_matches[1])
            else:
                # Check for "target 230 235" format
                target_pattern = r'target\s+([\d.]+)\s+([\d.]+)'
                double_target = re.search(target_pattern, msg_lower)
                if double_target:
                    target1 = float(double_target.group(1))
                    target2 = float(double_target.group(2))
        
        # Quick add mode
        if len(parts) == 2:
            idea = TradeIdea(
                ticker=ticker,
                source="dp",
                score=ConvictionScore(0.60, "dp", "Medium"),
                entry=entry,
                stop=stop,
                target1=target1,
                target2=target2 if target2 else target1
            )
            self.context.ideas.append(idea)
            return f"✅ Quick added {ticker} (DP/Medium)"
        
        # Determine source
        if parts[2].lower() in ["dp", "mancini"]:
            source = parts[2].lower()
            phrase = " ".join(parts[3:]).lower()
        else:
            if ticker in ["ES", "NQ", "RTY", "YM"]:
                source = "mancini"
            elif ticker == "SPX":
                return "❌ SPX requires source: 'add SPX dp focus trade' or 'add SPX mancini fb'"
            else:
                source = "dp"
            phrase = " ".join(parts[2:]).lower()
        
        # Score based on source
        if source == "dp":
            for conv_phrase, score, label in DP_CONVICTION_MAP:
                if conv_phrase in phrase:
                    idea = TradeIdea(
                        ticker=ticker,
                        source="dp",
                        score=ConvictionScore(score, "dp", label),
                        entry=entry,
                        stop=stop,
                        target1=target1,
                        target2=target2 if target2 else target1
                    )
                    self.context.ideas.append(idea)
                    response = f"✅ Added {ticker} from DP: {label} ({score:.2f})"
                    if entry and stop and target1:
                        response += f"\nEntry: ${entry:.2f} | Stop: ${stop:.2f} | T1: ${target1:.2f}"
                        if target2:
                            response += f" | T2: ${target2:.2f}"
                    return response
                    
        elif source == "mancini":
            for setup_phrase, (score, label) in MANCINI_SETUP_MAP.items():
                if setup_phrase in phrase:
                    idea = TradeIdea(
                        ticker=ticker,
                        source="mancini",
                        score=ConvictionScore(score, "mancini", label),
                        entry=entry,
                        stop=stop,
                        target1=target1,
                        target2=target2 if target2 else target1
                    )
                    self.context.ideas.append(idea)
                    response = f"✅ Added {ticker} from Mancini: {label} ({score:.2f})"
                    if entry and stop and target1:
                        response += f"\nEntry: ${entry:.2f} | Stop: ${stop:.2f} | T1: ${target1:.2f}"
                        if target2:
                            response += f" | T2: ${target2:.2f}"
                    return response
        
        return f"❌ No matching pattern found for '{phrase}'"
    
    def handle_move_stop(self, message: str) -> str:
        """Move stop loss for a position."""
        parts = message.split()
        if len(parts) < 4:
            return "Format: move stop AAPL 225.50"
            
        ticker = parts[2].upper()
        try:
            new_stop = float(parts[3])
        except ValueError:
            return "Invalid stop price"
            
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        if not pos:
            return f"No position in {ticker}"
            
        # Validate stop
        if pos.side == "long":
            if new_stop >= pos.current:
                return f"❌ Long stop must be below current price (${pos.current:.2f})"
        else:
            if new_stop <= pos.current:
                return f"❌ Short stop must be above current price (${pos.current:.2f})"
                
        old_stop = pos.stop
        pos.stop = new_stop
        
        response = f"✅ STOP MOVED: {ticker}\n"
        if old_stop:
            response += f"${old_stop:.2f} → ${new_stop:.2f}\n"
        else:
            response += f"Set to ${new_stop:.2f}\n"
        
        # Calculate risk
        if pos.side == "long":
            risk = pos.current - new_stop
        else:
            risk = new_stop - pos.current
            
        response += f"Risk: ${risk:.2f}/share ({(risk/pos.current)*100:.1f}%)"
        
        # Journal the move
        self.context.journal.append(
            f"[{datetime.now().isoformat()}] Moved {ticker} stop: ${old_stop:.2f} → ${new_stop:.2f}" 
            if old_stop else f"Set {ticker} stop: ${new_stop:.2f}"
        )
        
        return response
    
    def handle_lock_profits(self, message: str) -> str:
        """Lock 75% profits (Mancini rule)."""
        symbols = self._extract_symbols(message)
        if not symbols:
            mancini_pos = [p for p in self.context.positions if p.source == "mancini" and p.pnl > 0]
            if not mancini_pos:
                return "No profitable Mancini positions to lock"
            ticker = mancini_pos[0].ticker
        else:
            ticker = symbols[0]
            
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        if not pos:
            return f"No position in {ticker}"
            
        if pos.source != "mancini":
            return f"⚠️ 75% rule is for Mancini trades only. {ticker} is a DP trade."
            
        if pos.pnl <= 0:
            return f"❌ {ticker} not profitable yet"
            
        # Calculate 75% exit
        exit_qty = int(pos.qty * 0.75)
        remaining_qty = pos.qty - exit_qty
        exit_pnl = (pos.pnl / pos.qty) * exit_qty
        
        # Update position
        pos.qty = remaining_qty
        self.context.realized_pnl += exit_pnl
        
        response = f"✅ LOCKED 75% PROFITS: {ticker}\n"
        response += f"Sold {exit_qty} units for ${exit_pnl:+.2f}\n"
        response += f"Runner: {remaining_qty} units remain\n"
        response += "📋 Trail stop on runner to next level"
        
        return response
    
    def handle_coach(self, message: str) -> str:
        """Provide coaching feedback."""
        alerts = []
        prescriptions = []
        
        # Check for revenge trading
        if self.context.stops_hit >= 3:
            alerts.append("🛑 3+ stops hit - revenge trading risk HIGH")
            prescriptions.append("• Step away for 30 minutes")
            prescriptions.append("• Journal about the losses")
            prescriptions.append("• Return with half size only")
            
        # Check for overtrading
        if self.context.trades_completed > 10:
            alerts.append("⚠️ Overtrading detected (>10 trades)")
            prescriptions.append("• Focus on A+ setups only")
            prescriptions.append("• Quality over quantity")
            
        # Check conviction discipline
        low_conviction_trades = len([i for i in self.context.ideas if i.score.score < 0.50])
        if low_conviction_trades > 3:
            alerts.append("⚠️ Taking too many low conviction trades")
            prescriptions.append("• Minimum 0.70 score tomorrow")
            prescriptions.append("• Review your focus list")
            
        response = "=== COACH FEEDBACK ===\n"
        
        if alerts:
            response += "\nBEHAVIORAL ALERTS:\n"
            for alert in alerts:
                response += f"{alert}\n"
                
            response += "\nPRESCRIPTIONS:\n"
            for rx in prescriptions:
                response += f"{rx}\n"
        else:
            response += "\n✅ Good discipline today!\n"
            response += "• Keep following your plan\n"
            response += "• Size up on focus trades\n"
            response += "• Trust your analysis\n"
            
        # Tomorrow's focus
        response += "\nTOMORROW'S FOCUS:\n"
        response += "1. Wait for A+ setups only\n"
        response += "2. Respect source-based rules\n"
        response += "3. Honor stops without revenge\n"
        response += "4. Journal after each trade\n"
        
        self.context.phase = "PLAN"
        response += "\n-> Ready for next session (Phase: PLAN)"
        
        return response
    
    def handle_review(self, message: str) -> str:
        """Review session performance."""
        response = "=== SESSION REVIEW ===\n"
        response += f"Completed Trades: {self.context.trades_completed}\n"
        response += f"Realized P&L: ${self.context.realized_pnl:.2f}\n"
        response += f"Stops Hit: {self.context.stops_hit}\n"
        
        # Win rate
        if self.context.trades_completed > 0:
            win_rate = ((self.context.trades_completed - self.context.stops_hit) / self.context.trades_completed) * 100
            response += f"Win Rate: {win_rate:.0f}%\n"
            
        # Overall assessment
        if self.context.realized_pnl > 0:
            response += "\n✅ Positive session - good discipline"
        elif self.context.realized_pnl < 0:
            response += "\n❌ Negative session - review entries"
        else:
            response += "\n➖ Breakeven session"
            
        response += "\n\n-> Ready for COACH phase"
        self.context.phase = "COACH"
        
        return response
    
    def handle_save(self, message: str) -> str:
        """Save context to JSON."""
        filename = f"trader_{datetime.now().strftime('%Y%m%d_%H%M')}.json"
        
        # Convert to dict for JSON
        data = asdict(self.context)
        json_str = json.dumps(data, indent=2)
        
        return f"""✅ SESSION SAVED: {filename}

Copy this JSON to restore tomorrow:

```json
{json_str}
```

To restore: Start new conversation with "Initialize Intent Trader with context: [paste JSON]"
"""
    
    def handle_load(self, message: str) -> str:
        """Load context from JSON string."""
        json_match = re.search(r'(\{[\s\S]*\})', message)
        if not json_match:
            return "❌ No JSON context found. Format: load context: {paste JSON}"
            
        try:
            json_str = json_match.group(1)
            data = json.loads(json_str)
            
            # Reconstruct context
            self.context = TradingContext(**data)
            
            # Fix nested objects
            self.context.ideas = [TradeIdea(**idea) for idea in data.get('ideas', [])]
            for idea in self.context.ideas:
                if isinstance(idea.score, dict):
                    idea.score = ConvictionScore(**idea.score)
                # Restore status enum
                if hasattr(idea, '_status') and idea._status:
                    idea._status = TradeStatus(idea._status)
                    
            self.context.positions = [Position(**pos) for pos in data.get('positions', [])]
            
            return f"""✅ SESSION RESTORED

Phase: {self.context.phase}
Positions: {len(self.context.positions)}
Ideas: {len(self.context.ideas)}
P&L: ${self.context.realized_pnl:.2f}

Ready to continue trading!"""
            
        except Exception as e:
            return f"❌ Load failed: {str(e)}"
    
    def handle_journal(self, message: str) -> str:
        """Journal management."""
        parts = message.split(maxsplit=1)
        
        if len(parts) == 1:
            if not self.context.journal:
                return "Journal is empty"
            return "📓 RECENT JOURNAL:\n" + "\n".join(self.context.journal[-5:])
            
        # Add entry
        entry = f"[{datetime.now().isoformat()}] {parts[1]}"
        self.context.journal.append(entry)
        
        # Cleanup old entries
        if len(self.context.journal) > MAX_JOURNAL_ENTRIES:
            self.context.journal = self.context.journal[-MAX_JOURNAL_ENTRIES:]
            
        return f"✅ Journaled: {parts[1]}"
    
    def handle_note(self, message: str) -> str:
        """Quick note about a position."""
        parts = message.split(maxsplit=2)
        if len(parts) < 3:
            return "Format: note AAPL holding through earnings"
            
        ticker = parts[1].upper()
        note = parts[2]
        
        # Find position
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        if pos:
            entry = f"[{datetime.now().isoformat()}] {ticker} ({pos.source}): {note}"
        else:
            entry = f"[{datetime.now().isoformat()}] {ticker}: {note}"
            
        self.context.journal.append(entry)
        return f"✅ Noted: {ticker} - {note}"
    
    def handle_update(self, message: str) -> str:
        """Update position prices quickly."""
        parts = message.split()[1:]
        
        if not parts:
            return "Format: update AAPL 227.50 TSLA 185.20"
            
        updated = []
        i = 0
        while i < len(parts):
            if i + 1 < len(parts):
                ticker = parts[i].upper()
                try:
                    price = float(parts[i + 1])
                    # Find position
                    for pos in self.context.positions:
                        if pos.ticker == ticker:
                            pos.current = price
                            updated.append(f"{ticker} → ${price:.2f}")
                            break
                    i += 2
                except ValueError:
                    i += 1
            else:
                i += 1
                
        if updated:
            return "✅ Updated: " + ", ".join(updated)
        return "❌ No positions updated"
    
    def handle_quick(self, message: str) -> str:
        """Quick add without full details."""
        parts = message.split()
        if len(parts) < 2:
            return "Format: quick AAPL"
            
        ticker = parts[1].upper()
        
        # Find best idea for this ticker
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        if not ideas:
            return f"❌ No analysis for {ticker}. Run analysis first."
            
        best_idea = max(ideas, key=lambda i: i.score.score)
        
        position = Position(
            ticker=ticker,
            source=best_idea.source,
            side="long",
            qty=100,
            entry=100.0,  # Placeholder
            current=100.0,
            stop=None
        )
        
        self.context.positions.append(position)
        self.context.phase = "MANAGE"
        
        return f"✅ Quick added {ticker} ({best_idea.source.upper()}: {best_idea.score.label})"
    
    def handle_chart(self, message: str) -> str:
        """Decode chart visuals and link to actionable trades."""
        msg = message.lower()
        ticker = next((s for s in self._extract_symbols(message) if s not in ['FB', 'ES', 'YH', 'YL', 'HOD', 'LOD']), None)
        
        # Pattern detection
        patterns = {
            'fb|failed breakdown': ('MANCINI_FB', 0.85),
            'bull flag|bullflag': ('BULL_FLAG', 0.75),
            'bear flag|bearflag': ('BEAR_FLAG', 0.75),
            'reclaim': ('RECLAIM', 0.70),
            'megaphone': ('MEGAPHONE', 0.50),
            'ascending triangle': ('ASC_TRIANGLE', 0.70),
            'descending triangle': ('DESC_TRIANGLE', 0.70)
        }
        
        # Detect pattern
        detected_pattern = None
        pattern_score = 0
        for pattern_key, (pattern_name, score) in patterns.items():
            if any(p in msg for p in pattern_key.split('|')):
                detected_pattern = pattern_name
                pattern_score = score
                break
        
        # Traffic light momentum
        if 'above 8' in msg and 'above 21' in msg:
            momentum = '🟢 GREEN'
            bias = 'long'
        elif 'below 8' in msg and 'below 21' in msg:
            momentum = '🔴 RED'
            bias = 'short'
        else:
            momentum = '🟡 YELLOW'
            bias = 'neutral'
        
        response = f"=== CHART READ"
        if ticker:
            response += f": {ticker}"
        response += " ===\n\n"
        
        response += f"Momentum: {momentum}\n"
        
        if detected_pattern:
            response += f"Pattern: {detected_pattern} (score: {pattern_score})\n"
            
            # Auto-create high conviction ideas
            if ticker and pattern_score >= 0.70:
                source = 'mancini' if detected_pattern == 'MANCINI_FB' else 'dp'
                label = 'FB' if detected_pattern == 'MANCINI_FB' else ('High' if pattern_score >= 0.70 else 'Medium')
                
                idea = TradeIdea(
                    ticker=ticker,
                    source=source,
                    score=ConvictionScore(pattern_score, source, label),
                    notes=f"Chart pattern: {detected_pattern}"
                )
                self.context.ideas.append(idea)
                response += f"✅ Added to ideas (source: {source})\n"
        
        # Trade bias
        response += f"\nTRADE BIAS: "
        if momentum == '🟢 GREEN' and detected_pattern in ['BULL_FLAG', 'ASC_TRIANGLE', 'MANCINI_FB']:
            response += f"STRONG LONG - Size up"
        elif momentum == '🔴 RED' and detected_pattern in ['BEAR_FLAG', 'DESC_TRIANGLE']:
            response += f"STRONG SHORT - Size up"
        elif momentum == '🟡 YELLOW':
            response += "WAIT - Need momentum confirmation"
        else:
            response += f"STANDARD {bias.upper()} - Normal size"
        
        # Quick action
        if pattern_score >= 0.70 and ticker:
            response += f"\n\n→ Say 'buy {ticker}' to execute"
        
        return response
    
    def handle_focus_trades(self, message: str) -> str:
        """Show all focus trades from both sources."""
        dp_focus = [i for i in self.context.ideas if i.source == "dp" and i.score.score >= 0.90]
        mancini_focus = [i for i in self.context.ideas if i.source == "mancini" and i.score.score >= 0.85]
        
        if not dp_focus and not mancini_focus:
            return "No focus trades identified. Run analysis first."
            
        response = "=== TODAY'S FOCUS TRADES ===\n\n"
        
        if dp_focus:
            response += "DP FOCUS (0.90+):\n"
            for idea in dp_focus:
                entry = f" @ ${idea.entry:.2f}" if idea.entry else ""
                response += f"• {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
                
        if mancini_focus:
            response += "\nMANCINI FOCUS (0.85+):\n"
            for idea in mancini_focus:
                entry = f" @ ${idea.entry:.2f}" if idea.entry else ""
                response += f"• {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
                if idea.notes:
                    response += f"  → {idea.notes}\n"
                    
        return response
    
    def handle_check_source(self, message: str) -> str:
        """Check source for a specific ticker."""
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify ticker: 'check source SPX'"
            
        ticker = symbols[0]
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        
        if not ideas:
            if ticker in ["ES", "NQ", "RTY", "YM"]:
                return f"✅ {ticker} is a futures contract → Always use Mancini scoring"
            elif ticker == "SPX":
                return f"⚠️ {ticker} requires source verification. Found in both systems?"
            else:
                return f"✅ {ticker} is a stock/ETF → Always use DP scoring"
                
        response = f"=== SOURCE CHECK: {ticker} ===\n"
        for idea in ideas:
            response += f"• Source: {idea.source.upper()}\n"
            response += f"  Score: {idea.score.score:.2f} ({idea.score.label})\n"
            if idea.entry:
                response += f"  Entry: ${idea.entry:.2f}\n"
            if idea.notes:
                response += f"  Notes: {idea.notes}\n"
                
        return response
    
    def handle_size_position(self, message: str) -> str:
        """Calculate position size based on source rules."""
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify ticker: 'size AAPL'"
            
        ticker = symbols[0]
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        
        if not ideas:
            return f"No analysis for {ticker}. Run analysis first."
            
        best_idea = max(ideas, key=lambda i: i.score.score)
        
        response = f"=== POSITION SIZE: {ticker} ===\n"
        response += f"Source: {best_idea.source.upper()}\n"
        response += f"Score: {best_idea.score.score:.2f} ({best_idea.score.label})\n\n"
        
        if best_idea.source == "dp":
            if best_idea.score.score >= 0.90:
                response += "Size: FULL SIZE+ (Focus trade)\n"
            elif best_idea.score.score >= 0.70:
                response += "Size: FULL SIZE (High conviction)\n"
            elif best_idea.score.score >= 0.50:
                response += "Size: HALF SIZE (Medium conviction)\n"
            else:
                response += "Size: QUARTER SIZE (Low conviction)\n"
        else:  # mancini
            if best_idea.score.label == "FB":
                response += "Size: FULL SIZE (Failed Breakdown)\n"
            elif best_idea.score.score >= 0.70:
                response += "Size: FULL SIZE (Strong setup)\n"
            else:
                response += "Size: HALF SIZE (Weaker setup)\n"
                
        # Mode adjustment
        if self.context.mode == "Mode2":
            response += "\n⚠️ Mode 2 Market: Consider reducing size by 25%"
            
        return response
    
    def handle_performance(self, message: str) -> str:
        """Detailed performance analysis by source."""
        response = "=== PERFORMANCE BY SOURCE ===\n"
        
        # Analyze ideas by score buckets
        dp_ideas = [i for i in self.context.ideas if i.source == "dp"]
        mancini_ideas = [i for i in self.context.ideas if i.source == "mancini"]
        
        if dp_ideas:
            response += "\nDP/INNER CIRCLE:\n"
            response += "| CONVICTION | COUNT |\n"
            response += "|------------|-------|\n"
            exceptional = len([i for i in dp_ideas if i.score.score >= 0.90])
            high = len([i for i in dp_ideas if 0.70 <= i.score.score < 0.90])
            medium = len([i for i in dp_ideas if 0.50 <= i.score.score < 0.70])
            
            response += f"| Exceptional (0.90+) | {exceptional} |\n"
            response += f"| High (0.70-0.89) | {high} |\n"
            response += f"| Medium (0.50-0.69) | {medium} |\n"
            
        if mancini_ideas:
            response += "\nMANCINI BLUEPRINT:\n"
            response += "| SETUP TYPE | COUNT |\n"
            response += "|------------|-------|\n"
            fb = len([i for i in mancini_ideas if i.score.label == "FB"])
            reclaim = len([i for i in mancini_ideas if i.score.label == "Reclaim"])
            support = len([i for i in mancini_ideas if i.score.label == "Support"])
            
            response += f"| Failed Breakdowns | {fb} |\n"
            response += f"| Level Reclaims | {reclaim} |\n"
            response += f"| Support Tests | {support} |\n"
            
        response += f"\nMarket Mode: {self.context.mode}\n"
        response += f"Total P&L: ${self.context.realized_pnl:.2f}"
        
        return response
    
    def handle_daily_report(self, message: str) -> str:
        """Generate comprehensive daily report."""
        report = f"=== DAILY REPORT {datetime.now().strftime('%Y-%m-%d')} ===\n\n"
        
        # Morning Plan
        report += "MORNING PLAN:\n"
        if self.context.dp_analysis:
            report += f"DP Bias: {self.context.dp_analysis.get('bias', 'N/A')}\n"
        
        focus_trades = [i for i in self.context.ideas if i.score.score >= 0.90]
        if focus_trades:
            report += f"Focus Trades: {', '.join([i.ticker for i in focus_trades])}\n"
        
        # My Trades
        report += "\nMY EXECUTION:\n"
        all_positions = self.context.positions + self.context.closed_positions
        if all_positions:
            report += "| TICKER | PLANNED | P&L |\n"
            report += "|--------|---------|-----|\n"
            for pos in all_positions:
                planned = "✅" if any(i.ticker == pos.ticker for i in self.context.ideas) else "❌"
                report += f"| {pos.ticker:<6} | {planned:<7} | ${pos.pnl:+.2f} |\n"
        
        # Moderator Trades
        if self.context.moderator_trades:
            report += "\nMODERATOR ACTIVITY:\n"
            report += "| MODERATOR | ACTION | TICKER | PRICE |\n"
            report += "|-----------|--------|--------|-------|\n"
            for trade in self.context.moderator_trades[-10:]:  # Last 10 trades
                report += f"| {trade['moderator']:<9} | {trade['action']:<6} | {trade['ticker']:<6} | ${trade['price']:.2f} |\n"
        
        # Performance
        report += f"\nPERFORMANCE:\n"
        report += f"My P&L: ${self.context.realized_pnl:+.2f}\n"
        report += f"Trades: {self.context.trades_completed}\n"
        report += f"Stops Hit: {self.context.stops_hit}\n"
        
        if self.context.trades_completed > 0:
            win_rate = ((self.context.trades_completed - self.context.stops_hit) / self.context.trades_completed) * 100
            report += f"Win Rate: {win_rate:.0f}%\n"
        
        # Behavioral
        behavioral_score = 100
        if self.context.stops_hit >= 3:
            behavioral_score -= 30
        if self.context.trades_completed > 10:
            behavioral_score -= 20
            
        report += f"\nBEHAVIORAL SCORE: {behavioral_score}/100\n"
        
        return report
    
    def handle_log_moderator(self, message: str) -> str:
        """Log moderator trades: log mod DP bought AAPL 225"""
        parts = message.lower().split()
        if len(parts) < 6:
            return "Format: log mod DP bought AAPL 225"
        
        mod_name = parts[2].upper()
        action = parts[3]
        ticker = parts[4].upper()
        try:
            price = float(parts[5])
        except (IndexError, ValueError):
            price = 0
        
        trade = {
            "moderator": mod_name,
            "action": action,
            "ticker": ticker,
            "price": price,
            "timestamp": datetime.now().isoformat()
        }
        
        self.context.moderator_trades.append(trade)
        
        # Cleanup old trades
        if len(self.context.moderator_trades) > MAX_MODERATOR_TRADES:
            self.context.moderator_trades = self.context.moderator_trades[-MAX_MODERATOR_TRADES:]
        
        return f"✅ Logged: {mod_name} {action} {ticker} @ ${price:.2f}"
    
    def handle_export_day(self, message: str) -> str:
        """Export day's activity to markdown."""
        filename = f"trading_log_{datetime.now().strftime('%Y%m%d')}.md"
        content = self.handle_daily_report("")
        
        # Add journal entries
        if self.context.journal:
            content += "\n## JOURNAL ENTRIES\n"
            for entry in self.context.journal[-20:]:  # Last 20 entries
                content += f"• {entry}\n"
        
        return f"""✅ EXPORT READY

Copy this markdown:

```markdown
{content}
```

Save as: {filename}
"""
    
    def handle_reset(self, message: str) -> str:
        """Reset context."""
        self.context = TradingContext()
        return "✅ Context reset. Starting fresh in PLAN phase."
    
    def handle_context(self, message: str) -> str:
        """Show current context."""
        return f"""=== CURRENT CONTEXT ===
Phase: {self.context.phase}
Mode: {self.context.mode}
Ideas: {len(self.context.ideas)}
Positions: {len(self.context.positions)}
P&L: ${self.context.realized_pnl:.2f}
Stops Hit: {self.context.stops_hit}
Journal Entries: {len(self.context.journal)}"""
    
    def handle_market_mode(self, message: str) -> str:
        """Set or check market mode."""
        parts = message.split()
        
        if len(parts) == 2:  # Just "market mode"
            return f"Current Market Mode: {self.context.mode}"
            
        if len(parts) >= 3:
            mode = parts[2]
            if mode in ["1", "mode1"]:
                self.context.mode = "Mode1"
            elif mode in ["2", "mode2"]:
                self.context.mode = "Mode2"
            else:
                return "Set mode: 'market mode 1' or 'market mode 2'"
                
            return f"✅ Market Mode set to: {self.context.mode}"
            
        return "Usage: 'market mode' to check or 'market mode 1/2' to set"
    
    def handle_dp_focus(self, message: str) -> str:
        """Show only DP focus trades."""
        dp_focus = [i for i in self.context.ideas if i.source == "dp" and i.score.score >= 0.90]
        
        if not dp_focus:
            return "No DP focus trades (0.90+)"
            
        response = "=== DP FOCUS TRADES ===\n"
        for idea in dp_focus:
            entry = f" @ ${idea.entry:.2f}" if idea.entry else ""
            response += f"• {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
            
        return response
    
    def handle_mancini_setups(self, message: str) -> str:
        """Show Mancini setups."""
        mancini = [i for i in self.context.ideas if i.source == "mancini"]
        
        if not mancini:
            return "No Mancini setups identified"
            
        response = "=== MANCINI SETUPS ===\n"
        for idea in mancini:
            entry = f" @ ${idea.entry:.2f}" if idea.entry else ""
            response += f"• {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
            if idea.notes:
                response += f"  → {idea.notes}\n"
                
        return response
    
    def handle_behavioral_check(self, message: str) -> str:
        """Real-time behavioral check."""
        alert = self._check_behavioral_patterns()
        if alert:
            return alert
        return "✅ No behavioral issues detected"
    
    def handle_unknown(self, message: str) -> str:
        """Handle unknown messages."""
        return f"""❓ I didn't understand that. Say 'help' to see what I can do.

You're currently in {self.context.phase} phase.

Try commands like:
• analyze dp [text]
• show plan
• buy AAPL
• positions"""

# === COLOR SCHEMA ===

COLOR_SCHEMA = {
    "cyan":      ("8 MA",      "#00FFFF", (0, 255, 255)),
    "traffic":   ("21 MA",     "traffic_light", None),
    "blue":      ("50 MA",     "#0066FF", (0, 102, 255)),
    "orange":    ("100 MA",    "#FF7E00", (255, 126, 0)),
    "yellow":    ("200 MA",    "#FFF000", (255, 240, 0)),
    "magenta":   ("VWAP",      "#FF66FF", (255, 102, 255)),
    "purple":    ("AVWAP (YTD)", "#9370DB", (147, 112, 219)),
    "lightgray": ("Keltner Channels", "#B5B5B5", (181, 181, 181)),
    "white":     ("Trend lines/levels", "#FFFFFF", (255, 255, 255)),
}

# === MAIN EXECUTION ===

# Global trader instance
trader = None

def initialize_trader():
    """Initialize a new IntentTrader instance."""
    global trader
    trader = IntentTrader()
    return trader

def say(message: str) -> str:
    """Process a message through the trader."""
    global trader
    if trader is None:
        trader = IntentTrader()
    return trader.process(message)

# Example usage for AI assistants
def demo():
    """
    Demonstration of how to use IntentTrader with an AI Assistant.
    
    Example conversation:
    
    User: Initialize Intent Trader
    Assistant: [Creates new IntentTrader instance]
    
    User: analyze dp AAPL really like this setup above 225
    Assistant: [Processes with trader.process("analyze dp AAPL really like this setup above 225")]
    
    User: buy AAPL
    Assistant: [Processes with trader.process("buy AAPL")]
    """
    
    # Initialize trader
    t = initialize_trader()
    
    # Example messages
    examples = [
        "help",
        "analyze dp AAPL is a focus trade above 225, love this setup",
        "create plan",
        "buy 100 AAPL @ 225.50",
        "positions",
        "move stop AAPL 224.00",
        "exit AAPL"
    ]
    
    print("=== INTENT TRADER DEMO ===\n")
    
    for msg in examples:
        print(f"You: {msg}")
        response = say(msg)
        print(f"Trader:\n{response}\n")
        print("-" * 50 + "\n")
    
    return "Demo completed successfully"

# For local testing only - won't run in AI environments
if __name__ == "__main__":
    import sys
    
    # Check if running in interactive mode
    if hasattr(sys, 'ps1'):
        print("Interactive mode detected. Use demo() to see examples.")
    else:
        # If someone runs this file directly
        print("""
+------------------------------------------------+
|  Intent Trader v1.1.0 - AI Assistant Version   |
|                                                |
|  Enhanced with trade status tracking and       |
|  improved plan management.                     |
|                                                |
|  This version is designed for AI assistants.   |
|  For interactive use, import and use:          |
|  - initialize_trader() to start                |
|  - say(message) to execute commands            |
|  - demo() to see examples                      |
+------------------------------------------------+
        """)

# === HELPER CLASSES ===

class DPConvictionAnalyzer:
    """Helper for context-aware DP conviction scoring"""
    def __init__(self):
        self.amplifiers = ['really', 'absolutely', 'definitely', 'very', 'extremely', 'love', 'focus', 'aggressive']
        self.hedgers = ['maybe', 'might', 'possibly', 'perhaps', 'decent', 'kinda', 'sorta']
        self.negators = ['not', 'no', "don't", "doesn't", "isn't", "won't", "can't"]
    def analyze_line(self, line: str, ticker: str) -> dict:
        """Analyze a line for conviction with context awareness"""
        line_lower = line.lower()
        # Default result
        result = {
            'score': None,
            'label': None,
            'energy': 'LOW',
            'confidence': 0.5,
            'matched_phrase': None
        }
        # Check each conviction phrase
        for phrase, base_score, base_label in DP_CONVICTION_MAP:
            if phrase in line_lower:
                score = base_score
                label = base_label
                # Check for negation BEFORE the phrase
                before_phrase = line_lower.split(phrase)[0]
                words_before = before_phrase.split()[-3:]  # Last 3 words
                if any(neg in words_before for neg in self.negators):
                    # Special case: "no brainer" is positive
                    if phrase != "no brainer long":
                        score = 0.15
                        label = "Avoid"
                # Count modifiers
                amp_count = sum(1 for amp in self.amplifiers if amp in line_lower)
                hedge_count = sum(1 for hedge in self.hedgers if hedge in line_lower)
                # Apply amplifiers
                if amp_count > 0:
                    score = min(score * (1 + 0.05 * amp_count), 0.95)
                # Apply hedgers  
                if hedge_count > 0:
                    score = max(score * (1 - 0.05 * hedge_count), 0.25)
                # Determine energy
                if '!' in line or amp_count >= 2:
                    energy = 'HIGH'
                elif score >= 0.85 or amp_count > 0:
                    energy = 'MEDIUM'
                else:
                    energy = 'LOW'
                # Confidence based on match quality
                confidence = 0.9 if phrase in line_lower else 0.7
                if amp_count > 0 or hedge_count > 0:
                    confidence = min(confidence + 0.1, 1.0)
                result = {
                    'score': score,
                    'label': label,
                    'energy': energy,
                    'confidence': confidence,
                    'matched_phrase': phrase
                }
                break
        return result