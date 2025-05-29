"""
Intent Trader - AI Trading Assistant
Version: 1.0.0
Date: 2024-05-28
Author: Solo Trader
License: MIT

Description:
    An AI-powered trading assistant that provides structured trading workflow 
    with persistent memory across messages.

How to Use:
    1. Start a new conversation with your AI Assistant
    2. Paste this code or say "Initialize Intent Trader"
    3. The AI Assistant maintains your trading state throughout the day
    4. Use natural commands like "analyze dp", "buy AAPL", "show positions"

Features:
    - Complete PFEMRC workflow (Plan, Focus, Execute, Manage, Review, Coach)
    - Source-based scoring (no methodology mixing)
    - Real-time behavioral coaching and pattern detection
    - Position tracking with live P&L calculations
    - Journal and context persistence within conversation
    - Structured trading vs random chat

This is designed to enhance any AI Assistant for systematic trading 
with memory and structure.
"""

import re
import json
from datetime import datetime
from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional, Tuple, Any

# === DATA MODELS ===

@dataclass
class ConvictionScore:
    """Represents a conviction score from a specific source."""
    score: float  # 0.0 to 1.0
    source: str   # "dp" or "mancini"
    label: str    # "Exceptional", "High", "FB", etc.
    
@dataclass
class TradeIdea:
    """A single trade idea with source-specific scoring."""
    ticker: str
    source: str  # CRITICAL: determines scoring methodology
    score: ConvictionScore
    entry: Optional[float] = None
    stop: Optional[float] = None
    target: Optional[float] = None
    size: Optional[str] = None
    notes: Optional[str] = None
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

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

# === SCORING MAPS ===

DP_CONVICTION_MAP = [
    # (phrase, score, label)
    ("focus trade", 0.95, "Exceptional"),
    ("get aggressive", 0.95, "Exceptional"),
    ("love this", 0.93, "Exceptional"),
    ("back up the truck", 0.92, "Exceptional"),
    ("definitely want", 0.85, "High"),
    ("really like", 0.80, "High"),
    ("strong conviction", 0.80, "High"),
    ("i'm a fan", 0.75, "High"),
    ("i'm a buyer", 0.65, "Medium"),
    ("decent setup", 0.60, "Medium"),
    ("worth owning", 0.60, "Medium"),
    ("like it", 0.55, "Medium"),
    ("if viable", 0.45, "Low"),
    ("worth watching", 0.45, "Low"),
    ("might work", 0.40, "Low"),
    ("on my radar", 0.35, "Low"),
    ("not excited", 0.20, "Avoid"),
    ("avoid", 0.10, "Avoid"),
    ("stay away", 0.10, "Avoid"),
    ("no way", 0.05, "Avoid"),
]

MANCINI_SETUP_MAP = {
    "failed breakdown": (0.90, "FB"),
    "fb": (0.90, "FB"),
    "failed break": (0.85, "FB"),
    "level reclaim": (0.75, "Reclaim"),
    "reclaimed": (0.75, "Reclaim"),
    "support test": (0.65, "Support"),
    "testing support": (0.65, "Support"),
    "mode 2 range": (0.50, "Trap"),
    "range trade": (0.50, "Trap"),
    "against mode": (0.30, "Avoid"),
    "fighting trend": (0.25, "Avoid"),
}

# === MAIN INTENT TRADER ===

class IntentTrader:
    """Complete PFEMRC trading assistant with source-based scoring."""
    
    def __init__(self):
        self.context = TradingContext()
        self.handlers = self._register_handlers()
        
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
            
            # COACH Phase
            "coach": self.handle_coach,
            "behavioral": self.handle_behavioral_check,
            
            # Utilities
            "help": self.handle_help,
            "save": self.handle_save,
            "load": self.handle_load,
            "journal": self.handle_journal,
            "reset": self.handle_reset,
            "context": self.handle_context,
        }
    
    def process(self, message: str) -> str:
        """Main entry point - process any message."""
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
        """Analyze DP morning call with conviction scoring."""
        lines = message.split('\n')
        analysis = {
            "bias": "NEUTRAL",
            "ideas": [],
            "levels": [],
            "conviction_phrases": []
        }
        
        # Extract all content
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
            
        # Score each symbol mentioned
        found_count = 0
        for symbol in unique_symbols:
            # Find context around symbol
            for line in lines:
                if symbol in line.upper():
                    line_lower = line.lower()
                    
                    # Check conviction phrases
                    for phrase, score, label in DP_CONVICTION_MAP:
                        if phrase in line_lower:
                            idea = TradeIdea(
                                ticker=symbol,
                                source="dp",
                                score=ConvictionScore(score, "dp", label)
                            )
                            
                            # Extract price if mentioned
                            prices = self._extract_levels(line)
                            if prices:
                                idea.entry = prices[0]
                                
                            self.context.ideas.append(idea)
                            analysis["ideas"].append(f"{symbol}: {label} ({score})")
                            found_count += 1
                            
                            if score >= 0.90:
                                analysis["conviction_phrases"].append(line.strip())
                            break
        
        # Store analysis
        analysis["levels"] = levels
        self.context.dp_analysis = analysis
        self.context.phase = "PLAN"
        
        # Format response with conviction scores
        response = "=== DP ANALYSIS ===\n"
        response += f"Bias: {analysis['bias']}\n"
        response += f"Key Levels: {', '.join(map(str, levels[:5]))}\n"
        
        # Add discovery summary
        response += f"\nWHAT I FOUND:\n"
        response += f"* Analyzed {len(unique_symbols)} unique tickers\n"
        response += f"* Scored {found_count} trade ideas\n"
        response += f"* Detected {len(analysis['conviction_phrases'])} high conviction phrases\n"
        
        if self.context.ideas:
            response += "\nCONVICTION SCORING:\n"
            
            # Group by conviction level
            exceptional = [i for i in self.context.ideas 
                          if i.source == "dp" and i.score.score >= 0.90]
            high = [i for i in self.context.ideas 
                   if i.source == "dp" and 0.70 <= i.score.score < 0.90]
            medium = [i for i in self.context.ideas 
                     if i.source == "dp" and 0.50 <= i.score.score < 0.70]
            low = [i for i in self.context.ideas 
                  if i.source == "dp" and i.score.score < 0.50]
            
            if exceptional:
                response += "\nEXCEPTIONAL (0.90+) - Focus Trades:\n"
                for i in exceptional:
                    entry_str = f" @ {i.entry}" if i.entry else ""
                    response += f"  * {i.ticker}: {i.score.score:.2f} = \"{i.score.label}\"{entry_str}\n"
                    
            if high:
                response += "\nHIGH (0.70-0.89) - Full Size:\n"
                for i in high:
                    entry_str = f" @ {i.entry}" if i.entry else ""
                    response += f"  * {i.ticker}: {i.score.score:.2f} = \"{i.score.label}\"{entry_str}\n"
                    
            if medium:
                response += "\nMEDIUM (0.50-0.69) - Half Size:\n"
                for i in medium:
                    entry_str = f" @ {i.entry}" if i.entry else ""
                    response += f"  * {i.ticker}: {i.score.score:.2f} = \"{i.score.label}\"{entry_str}\n"
                    
            if low:
                response += "\nLOW (<0.50) - Avoid/Quarter:\n"
                for i in low:
                    entry_str = f" @ {i.entry}" if i.entry else ""
                    response += f"  * {i.ticker}: {i.score.score:.2f} = \"{i.score.label}\"{entry_str}\n"
                
        if analysis["conviction_phrases"]:
            response += "\nKey Phrases Detected:\n"
            for phrase in analysis["conviction_phrases"][:3]:
                response += f"  * \"{phrase}\"\n"
        
        # Help with discovery
        response += "\nWHAT TO SAY NEXT:\n"
        response += "* \"analyze mancini [newsletter]\" - Add Mancini setups\n"
        response += "* \"show me focus trades\" - See your 0.90+ trades\n"
        response += "* \"create my plan\" - Build unified trading plan\n"
        
        # Handle ambiguity naturally
        unrecognized = [s for s in unique_symbols if s not in [i.ticker for i in self.context.ideas]]
        if unrecognized:
            response += f"\nCouldn't score: {', '.join(unrecognized)}\n"
            response += "-> These tickers had no conviction language nearby"
                
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
        
        # Format response with scoring
        response = "=== MANCINI ANALYSIS ===\n"
        response += f"Market Mode: {analysis['mode']}\n"
        response += f"ES Levels: {', '.join(map(str, levels[:4]))}\n"
        
        if found_setups:
            response += "\nTECHNICAL SCORING:\n"
            
            # Group by score ranges
            primary = [(label, score, level) for label, score, level in found_setups if score >= 0.85]
            strong = [(label, score, level) for label, score, level in found_setups if 0.70 <= score < 0.85]
            moderate = [(label, score, level) for label, score, level in found_setups if 0.50 <= score < 0.70]
            weak = [(label, score, level) for label, score, level in found_setups if score < 0.50]
            
            if primary:
                response += "\nPRIMARY EDGE (0.85+) - Full Size:\n"
                for label, score, level in primary:
                    level_str = f" @ ES {level}" if level else ""
                    response += f"  * {label}: {score:.2f} = \"Primary Edge\"{level_str}\n"
                    if level:
                        response += f"    -> SPX equivalent: {level/10:.0f}\n"
                        
            if strong:
                response += "\nSTRONG (0.70-0.84) - Full Size:\n"
                for label, score, level in strong:
                    level_str = f" @ ES {level}" if level else ""
                    response += f"  * {label}: {score:.2f} = \"Strong Setup\"{level_str}\n"
                    
            if moderate:
                response += "\nMODERATE (0.50-0.69) - Half Size:\n"
                for label, score, level in moderate:
                    level_str = f" @ ES {level}" if level else ""
                    response += f"  * {label}: {score:.2f} = \"Moderate Setup\"{level_str}\n"
                    
            if weak:
                response += "\nWEAK (<0.50) - Avoid/Quarter:\n"
                for label, score, level in weak:
                    level_str = f" @ ES {level}" if level else ""
                    response += f"  * {label}: {score:.2f} = \"Weak Setup\"{level_str}\n"
                    
        # Mode-specific guidance
        if self.context.mode == "Mode2":
            response += "\nMode 2 Market: Expect chop, reduce size, tighter stops"
        else:
            response += "\nMode 1 Market: Trend day potential, can hold runners"
                
        response += "\n-> Next: Create unified plan"
        
        return response
    
    def handle_create_plan(self, message: str) -> str:
        """Create trading plan maintaining source separation."""
        dp_ideas = [i for i in self.context.ideas if i.source == "dp"]
        mancini_ideas = [i for i in self.context.ideas if i.source == "mancini"]
        
        response = "=== DAILY TRADING PLAN ===\n"
        response += f"Phase: {self.context.phase} -> FOCUS\n"
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
        
        # Rules reminder
        response += "\nEXECUTION RULES:\n"
        response += "* DP trades: Size by conviction score\n"
        response += "* Mancini trades: Wait for acceptance confirmation\n"
        response += "* Never mix scoring methodologies\n"
        response += "* Verify source before ANY SPX trade\n"
        
        self.context.phase = "FOCUS"
        response += "\n-> Phase updated to FOCUS"
        
        return response
    
    # === FOCUS PHASE HANDLERS ===
    
    def handle_focus_trades(self, message: str) -> str:
        """Show all focus trades from both sources."""
        dp_focus = [i for i in self.context.ideas if i.source == "dp" and i.score.score >= 0.90]
        mancini_focus = [i for i in self.context.ideas if i.source == "mancini" and i.score.score >= 0.85]
        
        if not dp_focus and not mancini_focus:
            return "X No focus trades identified. Run analysis first."
            
        response = "=== TODAY'S FOCUS TRADES ===\n\n"
        
        if dp_focus:
            response += "DP FOCUS (0.90+):\n"
            for idea in dp_focus:
                entry = f" @ {idea.entry}" if idea.entry else ""
                response += f"* {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
                
        if mancini_focus:
            response += "\nMANCINI FOCUS (0.85+):\n"
            for idea in mancini_focus:
                entry = f" @ {idea.entry}" if idea.entry else ""
                response += f"* {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
                if idea.notes:
                    response += f"  -> {idea.notes}\n"
                    
        return response
    
    def handle_check_source(self, message: str) -> str:
        """Check source for a specific ticker."""
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify ticker: 'check source SPX'"
            
        ticker = symbols[0]
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        
        if not ideas:
            # Determine by instrument type
            if ticker in ["ES", "NQ", "RTY", "YM"]:
                return f"OK {ticker} is a futures contract -> Always use Mancini scoring"
            elif ticker == "SPX":
                return f"! {ticker} requires source verification. Found in both systems?"
            else:
                return f"OK {ticker} is a stock/ETF -> Always use DP scoring"
                
        response = f"=== SOURCE CHECK: {ticker} ===\n"
        for idea in ideas:
            response += f"* Source: {idea.source.upper()}\n"
            response += f"  Score: {idea.score.score:.2f} ({idea.score.label})\n"
            if idea.entry:
                response += f"  Entry: {idea.entry}\n"
            if idea.notes:
                response += f"  Notes: {idea.notes}\n"
                
        return response
    
    # === EXECUTE PHASE HANDLERS ===
    
    def handle_add_trade(self, message: str) -> str:
        """Add a new trade idea with proper source routing."""
        # Try to parse: add TICKER source phrase
        parts = message.split()
        if len(parts) < 3:
            return "Usage: add AAPL dp love this setup"
            
        ticker = parts[1].upper()
        
        # Auto-detect source if not specified
        if parts[2].lower() in ["dp", "mancini"]:
            source = parts[2].lower()
            phrase = " ".join(parts[3:]).lower()
        else:
            # Auto-detect by instrument type
            if ticker in ["ES", "NQ", "RTY", "YM"]:
                source = "mancini"
            elif ticker == "SPX":
                return "! SPX requires source: 'add SPX dp focus trade' or 'add SPX mancini fb'"
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
                        score=ConvictionScore(score, "dp", label)
                    )
                    self.context.ideas.append(idea)
                    return f"OK Added {ticker} from DP: {label} ({score:.2f})"
                    
        elif source == "mancini":
            for setup_phrase, (score, label) in MANCINI_SETUP_MAP.items():
                if setup_phrase in phrase:
                    idea = TradeIdea(
                        ticker=ticker,
                        source="mancini",
                        score=ConvictionScore(score, "mancini", label)
                    )
                    self.context.ideas.append(idea)
                    return f"OK Added {ticker} from Mancini: {label} ({score:.2f})"
                    
        return f"X No matching pattern found for '{phrase}'"
    
    def handle_execute(self, message: str) -> str:
        """Execute a trade with source validation."""
        # Parse multiple formats: buy 100 AAPL @ 225.50, buy AAPL, add AAPL
        pattern = r'(?:buy|sell|long|short|add)\s+(?:(\d+)\s+)?([A-Z]+)(?:\s+@\s+|\s+at\s+)?([\d.]+)?'
        match = re.search(pattern, message, re.I)
        
        if not match:
            return "Format: buy 100 AAPL @ 225.50 or buy AAPL"
            
        qty_str, ticker, price_str = match.groups()
        
        # Handle 'add' as a buy signal
        action = message.split()[0].lower()
        if action == "add":
            action = "buy"
            
        side = "long" if action in ["buy", "long", "add"] else "short"
        
        # Default quantity and price
        qty = int(qty_str) if qty_str else 100  # Default 100 shares
        price = float(price_str) if price_str else 0.0  # Will update with current
        
        # Find source for this ticker
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        if not ideas:
            if ticker == "SPX":
                return "! SPX trade requires source verification. Which system?"
            # Auto-assign source
            source = "mancini" if ticker in ["ES", "NQ", "RTY", "YM"] else "dp"
        else:
            # Use highest scored idea's source
            best_idea = max(ideas, key=lambda i: i.score.score)
            source = best_idea.source
            
        # Create position
        position = Position(
            ticker=ticker,
            source=source,
            side=side,
            qty=qty,
            entry=price if price > 0 else 100.0,  # Placeholder if no price
            current=price if price > 0 else 100.0,
            stop=None
        )
        
        self.context.positions.append(position)
        self.context.phase = "MANAGE"
        
        response = f"=== EXECUTED ===\n"
        response += f"{side.upper()} {qty} {ticker}"
        if price > 0:
            response += f" @ {price}"
        response += f"\nSource: {source.upper()}\n"
        response += f"Phase -> MANAGE\n"
        
        # Add management rules based on source
        if source == "mancini":
            response += "\nMancini Rules:\n"
            response += "* Lock 75% at first target\n"
            response += "* Trail runner to next level\n"
        else:
            response += "\nDP Rules:\n"
            response += "* Flexible management\n"
            response += "* Adjust on sentiment\n"
            
        return response
    
    def handle_size_position(self, message: str) -> str:
        """Calculate position size based on source rules."""
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify ticker: 'size AAPL risk 500'"
            
        ticker = symbols[0]
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        
        if not ideas:
            return f"X No analysis for {ticker}. Run analysis first."
            
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
            response += "\n! Mode 2 Market: Consider reducing size"
            
        return response
    
    # === MANAGE PHASE HANDLERS ===
    
    def handle_positions(self, message: str) -> str:
        """Show current positions with P&L."""
        if not self.context.positions:
            return "No open positions"
            
        response = "=== OPEN POSITIONS ===\n"
        total_unrealized = 0
        
        # Group by source
        dp_positions = [p for p in self.context.positions if p.source == "dp"]
        mancini_positions = [p for p in self.context.positions if p.source == "mancini"]
        
        if dp_positions:
            response += "\nDP POSITIONS:\n"
            for pos in dp_positions:
                response += f"* {pos.side.upper()} {pos.ticker} {pos.qty}@{pos.entry:.2f}"
                response += f" -> ${pos.pnl:+.2f} ({pos.pnl_pct:+.1f}%)\n"
                total_unrealized += pos.pnl
                
        if mancini_positions:
            response += "\nMANCINI POSITIONS:\n"
            for pos in mancini_positions:
                response += f"* {pos.side.upper()} {pos.ticker} {pos.qty}@{pos.entry:.2f}"
                response += f" -> ${pos.pnl:+.2f} ({pos.pnl_pct:+.1f}%)\n"
                total_unrealized += pos.pnl
                
        response += f"\nUnrealized: ${total_unrealized:+.2f}\n"
        response += f"Realized: ${self.context.realized_pnl:.2f}\n"
        response += f"Total P&L: ${self.context.realized_pnl + total_unrealized:+.2f}"
        
        return response
    
    def handle_lock_profits(self, message: str) -> str:
        """Lock 75% profits (Mancini rule)."""
        symbols = self._extract_symbols(message)
        if not symbols:
            # Find all Mancini positions
            mancini_pos = [p for p in self.context.positions if p.source == "mancini" and p.pnl > 0]
            if not mancini_pos:
                return "X No profitable Mancini positions to lock"
                
            ticker = mancini_pos[0].ticker
        else:
            ticker = symbols[0]
            
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        if not pos:
            return f"X No position in {ticker}"
            
        if pos.source != "mancini":
            return f"! 75% rule is for Mancini trades only. {ticker} is a DP trade."
            
        if pos.pnl <= 0:
            return f"X {ticker} not profitable yet"
            
        # Calculate 75% exit
        exit_qty = int(pos.qty * 0.75)
        remaining_qty = pos.qty - exit_qty
        exit_pnl = (pos.pnl / pos.qty) * exit_qty
        
        # Update position
        pos.qty = remaining_qty
        self.context.realized_pnl += exit_pnl
        
        response = f"=== LOCKED 75% PROFITS ===\n"
        response += f"{ticker}: Sold {exit_qty} units\n"
        response += f"Realized: ${exit_pnl:+.2f}\n"
        response += f"Runner: {remaining_qty} units remain\n"
        response += "Trail stop on runner to next level"
        
        return response
    
    def handle_move_stop(self, message: str) -> str:
        """Move stop loss for a position based on source rules."""
        # Parse format: move stop AAPL 225.50
        parts = message.split()
        if len(parts) < 4:
            return "Format: move stop AAPL 225.50"
            
        ticker = parts[2].upper()
        try:
            new_stop = float(parts[3])
        except ValueError:
            return "X Invalid stop price"
            
        # Find position
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        if not pos:
            return f"X No position in {ticker}"
            
        # Validate stop based on source rules
        if pos.side == "long":
            if new_stop >= pos.current:
                return f"X Long stop must be below current price (${pos.current:.2f})"
            if pos.stop and new_stop < pos.stop:
                return f"! Moving stop down from ${pos.stop:.2f} to ${new_stop:.2f} - are you sure?"
        else:  # short
            if new_stop <= pos.current:
                return f"X Short stop must be above current price (${pos.current:.2f})"
            if pos.stop and new_stop > pos.stop:
                return f"! Moving stop up from ${pos.stop:.2f} to ${new_stop:.2f} - are you sure?"
                
        # Apply source-specific rules
        old_stop = pos.stop
        pos.stop = new_stop
        
        response = f"=== STOP ADJUSTED ===\n"
        response += f"{ticker} ({pos.source.upper()})\n"
        if old_stop:
            response += f"Old Stop: ${old_stop:.2f}\n"
        response += f"New Stop: ${new_stop:.2f}\n"
        response += f"Current: ${pos.current:.2f}\n"
        
        # Calculate risk
        if pos.side == "long":
            risk_per_share = pos.current - new_stop
            risk_pct = (risk_per_share / pos.current) * 100
        else:
            risk_per_share = new_stop - pos.current
            risk_pct = (risk_per_share / pos.current) * 100
            
        response += f"Risk: ${risk_per_share:.2f}/share ({risk_pct:.1f}%)\n"
        
        # Source-specific guidance
        if pos.source == "mancini":
            response += "\nMancini Rules:\n"
            response += "* Never let winner go red\n"
            response += "* Trail to next level after 75% lock\n"
        else:
            response += "\nDP Rules:\n"
            response += "* Adjust based on sentiment\n"
            response += "* Wider stops for focus trades\n"
            
        # Journal the stop move
        journal_entry = f"Moved {ticker} stop: ${old_stop:.2f} -> ${new_stop:.2f}" if old_stop else f"Set {ticker} stop: ${new_stop:.2f}"
        self.context.journal.append(f"[{datetime.now().isoformat()}] {journal_entry}")
        
        return response
    
    def handle_exit(self, message: str) -> str:
        """Exit a position."""
        if "all" in message.lower():
            if not self.context.positions:
                return "X No positions to exit"
                
            total_pnl = sum(p.pnl for p in self.context.positions)
            self.context.realized_pnl += total_pnl
            self.context.trades_completed += len(self.context.positions)
            
            # Track stops hit
            stops_hit = sum(1 for p in self.context.positions if p.pnl < 0)
            self.context.stops_hit += stops_hit
            
            self.context.positions = []
            self.context.phase = "REVIEW"
            
            return f"OK CLOSED ALL: P&L ${total_pnl:+.2f}\n-> Phase: REVIEW"
            
        # Exit specific position
        symbols = self._extract_symbols(message)
        if not symbols:
            return "Specify: 'exit AAPL' or 'exit all'"
            
        ticker = symbols[0]
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        
        if not pos:
            return f"X No position in {ticker}"
            
        # Update price if provided
        levels = self._extract_levels(message)
        if levels:
            pos.current = levels[0]
            
        # Close position
        self.context.positions.remove(pos)
        self.context.realized_pnl += pos.pnl
        self.context.trades_completed += 1
        
        if pos.pnl < 0:
            self.context.stops_hit += 1
            
        response = f"=== CLOSED POSITION ===\n"
        response += f"{ticker}: ${pos.pnl:+.2f} ({pos.pnl_pct:+.1f}%)\n"
        response += f"Source: {pos.source.upper()}\n"
        
        if pos.pnl < 0:
            response += "X Stop hit"
        else:
            response += "OK Profit taken"
            
        if not self.context.positions:
            self.context.phase = "REVIEW"
            response += "\n\n-> All flat. Phase: REVIEW"
            
        return response
    
    # === REVIEW PHASE HANDLERS ===
    
    def handle_review(self, message: str) -> str:
        """Review session performance."""
        response = "=== SESSION REVIEW ===\n"
        response += f"Completed Trades: {self.context.trades_completed}\n"
        response += f"Realized P&L: ${self.context.realized_pnl:.2f}\n"
        response += f"Stops Hit: {self.context.stops_hit}\n"
        
        # Performance by source
        dp_trades = len([i for i in self.context.ideas if i.source == "dp"])
        mancini_trades = len([i for i in self.context.ideas if i.source == "mancini"])
        
        if dp_trades > 0:
            response += f"\nDP Ideas Generated: {dp_trades}"
        if mancini_trades > 0:
            response += f"\nMancini Setups Found: {mancini_trades}"
            
        # Win rate if available
        if self.context.trades_completed > 0:
            win_rate = ((self.context.trades_completed - self.context.stops_hit) / self.context.trades_completed) * 100
            response += f"\n\nWin Rate: {win_rate:.0f}%"
            
        # Overall assessment
        if self.context.realized_pnl > 0:
            response += "\n\nOK Positive session - good discipline"
        elif self.context.realized_pnl < 0:
            response += "\n\n! Negative session - review entries"
        else:
            response += "\n\n- Breakeven session"
            
        response += "\n\n-> Ready for COACH phase"
        self.context.phase = "COACH"
        
        return response
    
    def handle_performance(self, message: str) -> str:
        """Detailed performance analysis by source."""
        response = "=== PERFORMANCE BY SOURCE ===\n"
        
        # Analyze ideas by score buckets
        dp_ideas = [i for i in self.context.ideas if i.source == "dp"]
        mancini_ideas = [i for i in self.context.ideas if i.source == "mancini"]
        
        if dp_ideas:
            response += "\nDP/INNER CIRCLE:\n"
            exceptional = len([i for i in dp_ideas if i.score.score >= 0.90])
            high = len([i for i in dp_ideas if 0.70 <= i.score.score < 0.90])
            medium = len([i for i in dp_ideas if 0.50 <= i.score.score < 0.70])
            
            response += f"* Exceptional (0.90+): {exceptional}\n"
            response += f"* High (0.70-0.89): {high}\n"
            response += f"* Medium (0.50-0.69): {medium}\n"
            
        if mancini_ideas:
            response += "\nMANCINI BLUEPRINT:\n"
            fb = len([i for i in mancini_ideas if i.score.label == "FB"])
            reclaim = len([i for i in mancini_ideas if i.score.label == "Reclaim"])
            support = len([i for i in mancini_ideas if i.score.label == "Support"])
            
            response += f"* Failed Breakdowns: {fb}\n"
            response += f"* Level Reclaims: {reclaim}\n"
            response += f"* Support Tests: {support}\n"
            
        response += f"\nMarket Mode: {self.context.mode}"
        response += f"\nTotal P&L: ${self.context.realized_pnl:.2f}"
        
        return response
    
    # === COACH PHASE HANDLERS ===
    
    def handle_coach(self, message: str) -> str:
        """Provide coaching feedback."""
        alerts = []
        prescriptions = []
        
        # Check for revenge trading
        if self.context.stops_hit >= 3:
            alerts.append("!! 3+ stops hit - revenge trading risk HIGH")
            prescriptions.append("* Step away for 30 minutes")
            prescriptions.append("* Journal about the losses")
            prescriptions.append("* Return with half size only")
            
        # Check for overtrading
        if self.context.trades_completed > 10:
            alerts.append("! Overtrading detected (>10 trades)")
            prescriptions.append("* Focus on A+ setups only")
            prescriptions.append("* Quality over quantity")
            
        # Check conviction discipline
        low_conviction_trades = len([i for i in self.context.ideas if i.score.score < 0.50])
        if low_conviction_trades > 3:
            alerts.append("! Taking too many low conviction trades")
            prescriptions.append("* Minimum 0.70 score tomorrow")
            prescriptions.append("* Review your focus list")
            
        response = "=== COACH FEEDBACK ===\n"
        
        if alerts:
            response += "\nBEHAVIORAL ALERTS:\n"
            for alert in alerts:
                response += f"{alert}\n"
                
            response += "\nPRESCRIPTIONS:\n"
            for rx in prescriptions:
                response += f"{rx}\n"
        else:
            response += "\nOK Good discipline today!\n"
            response += "* Keep following your plan\n"
            response += "* Size up on focus trades\n"
            response += "* Trust your analysis\n"
            
        # Tomorrow's focus
        response += "\nTOMORROW'S FOCUS:\n"
        response += "1. Wait for A+ setups only\n"
        response += "2. Respect source-based rules\n"
        response += "3. Honor stops without revenge\n"
        response += "4. Journal after each trade\n"
        
        self.context.phase = "PLAN"
        response += "\n-> Ready for next session (Phase: PLAN)"
        
        return response
    
    def handle_behavioral_check(self, message: str) -> str:
        """Real-time behavioral check."""
        alert = self._check_behavioral_patterns()
        if alert:
            return alert
        return "OK No behavioral issues detected"
    
    # === UTILITY HANDLERS ===
    
    def handle_help(self, message: str) -> str:
        """Show help."""
        return """
INTENT TRADER - Natural Language Trading

=== HOW TO TALK TO ME ===
Just say what you want to do naturally:
"analyze dp" - I'll analyze DP's morning call
"buy AAPL" - I'll buy 100 shares of AAPL
"show positions" - I'll show your current positions

=== MORNING PLANNING ===
"analyze dp [paste morning call]" - Extract DP trade ideas
"analyze mancini [paste newsletter]" - Extract Mancini setups
"what's the market mode?" - Check current mode
"create my trading plan" - Build today's plan

=== PRE-MARKET FOCUS ===
"show me focus trades" - See all high conviction
"what are the dp focus trades?" - DP only
"show mancini setups" - Mancini only
"check source for SPX" - Verify ticker source

=== TRADING HOURS ===
"buy 100 AAPL at 225.50" - Specific entry
"buy AAPL" - Quick 100 share entry
"quick add AAPL" - Fast position add
"what size for AAPL?" - Position sizing help

=== MANAGING POSITIONS ===
"show my positions" - Current P&L status
"update AAPL to 227.50" - Update prices
"lock 75% on ES" - Mancini profit rule
"move stop on AAPL to 224" - Adjust stops
"exit AAPL" or "exit all" - Close positions
"note AAPL holding overnight" - Add notes

=== AFTER HOURS ===
"review my day" - Session summary
"show performance" - Detailed statistics

=== ANYTIME ===
"coach me" - Behavioral feedback
"behavioral check" - Pattern detection
"save session" - Export to JSON
"journal rough morning" - Add notes
"help" - This message
"reset" - Start fresh
"show context" - Current state

Currently in """ + self.context.phase + """ phase"""
    
    def handle_save(self, message: str) -> str:
        """Save context to JSON string for copy/paste."""
        filename = f"trader_{datetime.now().strftime('%Y%m%d_%H%M')}.json"
        
        # Convert to dict for JSON
        data = asdict(self.context)
        json_str = json.dumps(data, indent=2)
        
        # Also append to journal
        journal_entry = f"Session saved: {filename}"
        self.context.journal.append(f"[{datetime.now().isoformat()}] {journal_entry}")
        
        return f"""SESSION SAVED
        
Copy this JSON to restore tomorrow:

```json
{json_str}
```

To restore: Start new conversation with "Initialize Intent Trader with context: [paste JSON]"
"""
    
    def handle_load(self, message: str) -> str:
        """Load context from JSON string."""
        # Extract JSON from message
        import re
        json_match = re.search(r'(\{[\s\S]*\})', message)
        
        if not json_match:
            return """X No JSON context found. 
            
To load a saved session:
1. Copy your saved JSON
2. Say: "load context: {paste JSON here}"
"""
        
        try:
            json_str = json_match.group(1)
            data = json.loads(json_str)
            
            # Reconstruct context
            self.context = TradingContext(**data)
            
            # Fix nested objects
            self.context.ideas = [TradeIdea(**idea) for idea in data.get('ideas', [])]
            for idea in self.context.ideas:
                idea.score = ConvictionScore(**idea.score)
                
            self.context.positions = [Position(**pos) for pos in data.get('positions', [])]
            
            return f"""OK SESSION RESTORED
            
Phase: {self.context.phase}
Positions: {len(self.context.positions)}
Ideas: {len(self.context.ideas)}
P&L: ${self.context.realized_pnl:.2f}

Ready to continue trading!"""
            
        except Exception as e:
            return f"X Load failed: {str(e)}"
    
    def handle_journal(self, message: str) -> str:
        """Journal management."""
        parts = message.split(maxsplit=1)
        
        if len(parts) == 1:
            # Show last 5 entries
            if not self.context.journal:
                return "Journal is empty"
            return "RECENT JOURNAL:\n" + "\n".join(self.context.journal[-5:])
            
        # Add entry
        entry = f"[{datetime.now().isoformat()}] {parts[1]}"
        self.context.journal.append(entry)
        return f"OK Journaled: {parts[1]}"
    
    def handle_reset(self, message: str) -> str:
        """Reset context."""
        self.context = TradingContext()
        return "OK Context reset. Starting fresh in PLAN phase."
    
    def handle_context(self, message: str) -> str:
        """Show current context."""
        return f"""
CURRENT CONTEXT
Phase: {self.context.phase}
Mode: {self.context.mode}
Ideas: {len(self.context.ideas)}
Positions: {len(self.context.positions)}
P&L: ${self.context.realized_pnl:.2f}
Stops Hit: {self.context.stops_hit}
Journal Entries: {len(self.context.journal)}
"""
    
    def handle_update(self, message: str) -> str:
        """Update position prices quickly."""
        # Format: update AAPL 227.50 TSLA 185.20
        parts = message.split()[1:]  # Skip 'update'
        
        if not parts:
            return "Format: update AAPL 227.50 or update all"
            
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
                            updated.append(f"{ticker} -> {price}")
                            break
                    i += 2
                except ValueError:
                    i += 1
            else:
                i += 1
                
        if updated:
            return "OK Updated: " + ", ".join(updated)
        return "X No positions updated"
    
    def handle_quick(self, message: str) -> str:
        """Quick add without full details."""
        # Format: quick AAPL or q AAPL
        parts = message.split()
        if len(parts) < 2:
            return "Format: quick AAPL"
            
        ticker = parts[1].upper()
        
        # Find best idea for this ticker
        ideas = [i for i in self.context.ideas if i.ticker == ticker]
        if not ideas:
            return f"X No analysis for {ticker}. Run analysis first."
            
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
        
        return f"OK Quick added {ticker} ({best_idea.source.upper()}: {best_idea.score.label})"
    
    def handle_note(self, message: str) -> str:
        """Quick note about a position."""
        # Format: note AAPL holding through earnings
        parts = message.split(maxsplit=2)
        if len(parts) < 3:
            return "Format: note AAPL your note here"
            
        ticker = parts[1].upper()
        note = parts[2]
        
        # Find position
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        if pos:
            # Add to journal with position context
            entry = f"[{datetime.now().isoformat()}] {ticker} ({pos.source}): {note}"
            self.context.journal.append(entry)
            return f"OK Noted: {ticker} - {note}"
        else:
            # Just journal it
            entry = f"[{datetime.now().isoformat()}] {ticker}: {note}"
            self.context.journal.append(entry)
            return f"OK Noted: {note}"
    
    def handle_market_mode(self, message: str) -> str:
        """Set or check market mode."""
        parts = message.split()
        
        if len(parts) == 2:  # Just "market mode"
            return f"Current Market Mode: {self.context.mode}"
            
        if len(parts) >= 3:
            # Set mode: market mode 1 or market mode 2
            mode = parts[2]
            if mode in ["1", "mode1"]:
                self.context.mode = "Mode1"
            elif mode in ["2", "mode2"]:
                self.context.mode = "Mode2"
            else:
                return "Set mode: 'market mode 1' or 'market mode 2'"
                
            return f"OK Market Mode set to: {self.context.mode}"
            
        return "Usage: 'market mode' to check or 'market mode 1/2' to set"
    
    def handle_dp_focus(self, message: str) -> str:
        """Show only DP focus trades."""
        dp_focus = [i for i in self.context.ideas if i.source == "dp" and i.score.score >= 0.90]
        
        if not dp_focus:
            return "X No DP focus trades (0.90+)"
            
        response = "=== DP FOCUS TRADES ===\n"
        for idea in dp_focus:
            entry = f" @ {idea.entry}" if idea.entry else ""
            response += f"* {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
            
        return response
    
    def handle_mancini_setups(self, message: str) -> str:
        """Show Mancini setups."""
        mancini = [i for i in self.context.ideas if i.source == "mancini"]
        
        if not mancini:
            return "X No Mancini setups identified"
            
        response = "=== MANCINI SETUPS ===\n"
        for idea in mancini:
            entry = f" @ {idea.entry}" if idea.entry else ""
            response += f"* {idea.ticker}: {idea.score.label} ({idea.score.score:.2f}){entry}\n"
            if idea.notes:
                response += f"  -> {idea.notes}\n"
                
        return response
    
    def handle_unknown(self, message: str) -> str:
        """Handle unknown messages."""
        return f"? I didn't understand that. Say 'help' to see what I can do.\nYou're currently in {self.context.phase} phase."
    
    # === HELPER METHODS ===
    
    def _extract_symbols(self, text: str) -> List[str]:
        """Extract stock symbols from text."""
        # Match 2-5 letter uppercase words
        symbols = re.findall(r'\b[A-Z]{2,5}\b', text)
        
        # Exclude common words
        exclude = {'THE', 'AND', 'FOR', 'BUY', 'SELL', 'LONG', 'SHORT', 'AT', 'TO', 'BE', 'IS', 'ON', 'IN', 'WITH'}
        
        return [s for s in symbols if s not in exclude]
    
    def _extract_levels(self, text: str) -> List[float]:
        """Extract price levels from text."""
        # Remove commas
        text = text.replace(',', '')
        
        # Find patterns like 5750, 575.50, etc
        prices = re.findall(r'\b(\d{2,6}(?:\.\d{1,2})?)\b', text)
        
        levels = []
        for p in prices:
            try:
                val = float(p)
                # Reasonable price range
                if 10 < val < 99999:
                    levels.append(val)
            except:
                pass
                
        return sorted(set(levels))
    
    def _check_behavioral_patterns(self) -> Optional[str]:
        """Check for behavioral issues in real-time."""
        if self.context.stops_hit >= 2 and len(self.context.positions) > 2:
            return "!! COACH ALERT: Overtrading after stops! Reduce size or step away."
            
        if self.context.stops_hit >= 3:
            return "!! COACH ALERT: 3 stops hit - Maximum risk reached. No new trades!"
            
        # Check for low quality trades
        if self.context.positions:
            active_ideas = []
            for pos in self.context.positions:
                idea = next((i for i in self.context.ideas if i.ticker == pos.ticker), None)
                if idea:
                    active_ideas.append(idea)
                    
            if active_ideas and all(i.score.score < 0.50 for i in active_ideas):
                return "! COACH ALERT: All positions are low conviction. Raise your standards!"
                
        return None


# === MAIN EXECUTION ===

# Global trader instance for AI assistants to use
trader = None

def initialize_trader():
    """Initialize a new IntentTrader instance."""
    global trader
    trader = IntentTrader()
    return trader

def say(message: str) -> str:
    """Just say what you want naturally."""
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
|  Intent Trader v1.0 - AI Assistant Version     |
|                                                |
|  This version is designed for AI assistants.   |
|  For interactive use, modify the main() func.  |
|                                                |
|