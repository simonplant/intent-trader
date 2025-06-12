"""
Intent Trader - AI Trading Assistant (Conservative Optimization)
Version: 0.4.3
Updated: 2024-06-12
Author: Simon Plant
License: MIT

Intent Trader is a trading assistant that uses the Plan-Focus-Execute-Manage-Review-Coach framework to plan and execute trade ideas.
"""

import re
import json
from datetime import datetime
from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional, Tuple, Any
from enum import Enum
import inspect
import os
from pathlib import Path

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
PRICE_ALERT_THRESHOLD = 0.01

# Limits
MAX_CLOSED_POSITIONS = 100
MAX_JOURNAL_ENTRIES = 500
MAX_MODERATOR_TRADES = 100

# Global override for ES-SPX offset (None = auto, int = manual value)
manual_override_offset: Optional[int] = None

def get_es_spx_offset(as_of: Optional[datetime] = None) -> int:
    """Calculate the current ES-SPX offset based on time decay.
    
    The offset decays linearly from ~40 points at contract start to ~5 points
    at expiry, based on the quarterly roll cycle.
    
    Args:
        as_of: Optional datetime to calculate offset for (defaults to now)
        
    Returns:
        int: The current offset in points
    """
    if manual_override_offset is not None:
        return manual_override_offset
        
    if as_of is None:
        as_of = datetime.today()

    # Quarterly roll dates (3rd Friday of Mar/Jun/Sep/Dec)
    expiry_dates = [
        datetime(as_of.year, 3, 15),
        datetime(as_of.year, 6, 21),
        datetime(as_of.year, 9, 20),
        datetime(as_of.year, 12, 20),
    ]

    # Find the next expiry and start
    for i, expiry in enumerate(expiry_dates):
        if as_of <= expiry:
            start = expiry_dates[i - 1] if i > 0 else datetime(as_of.year - 1, 12, 15)
            days_total = (expiry - start).days
            days_left = (expiry - as_of).days
            decay = max(0, min(1.0, days_left / days_total))
            offset = round(5 + 35 * decay)  # Linear decay from 40 → 5
            return offset

    return 5  # Fallback

# === SHARED HELPER FUNCTIONS ===

def size_for_score(score: float, mode: str = "Mode2") -> int:
    """Return position size based on conviction score and market mode."""
    if score >= DP_FOCUS_THRESHOLD:
        base = FOCUS_TRADE_SIZE
    elif score >= DP_HIGH_THRESHOLD:
        base = HIGH_CONVICTION_SIZE
    elif score >= DP_MEDIUM_THRESHOLD:
        base = MEDIUM_CONVICTION_SIZE
    else:
        base = LOW_CONVICTION_SIZE

    # Risk-downshift in Mode 2 markets
    if mode == "Mode2":
        base = int(base * 0.75)
    
    # Ensure minimum position size of 1 share
    return max(base, 1)

def format_plan_table(ideas: List['TradeIdea']) -> str:
    """Return a unified markdown table for any collection of TradeIdea objects.
    
    Columns:
      TICKER | SOURCE | TIER | SCORE | STATUS | ENTRY | STOP | T1 | T2 | R:R
    All ideas are shown with the same columns so DP and Mancini ideas can be
    compared side-by-side.  Missing data are rendered as "---".
    """
    if not ideas:
        return "No trade ideas match this filter.\n"

    # Sort – higher score first then ticker for stable display
    ideas_sorted = sorted(ideas, key=lambda x: (-x.score.score, x.ticker))

    table_lines = [
        "| TICKER | SOURCE | TIER | SCORE | STATUS | ENTRY | STOP | T1 | T2 | R:R |",
        "|--------|--------|------|-------|--------|-------|------|----|----|-----|",
    ]

    for idea in ideas_sorted:
        tier_str = str(getattr(idea, "tier_level", "-")) if idea.source == "mancini" else "-"
        entry_str = f"{idea.entry:.2f}" if idea.entry else "---"
        stop_str = f"{idea.stop:.2f}" if idea.stop else "---"
        t1_str = f"{idea.target1:.2f}" if idea.target1 else "---"
        t2_str = f"{idea.target2:.2f}" if idea.target2 else "---"
        rr_str = f"{idea.risk_reward:.1f}:1" if idea.risk_reward > 0 else "---"

        table_lines.append(
            f"| {idea.ticker:<6} | {idea.source:<6} | {tier_str:<4} | {idea.score.score:.2f} | "
            f"{idea.status.value:<10} | {entry_str:<5} | {stop_str:<5} | {t1_str:<5} | {t2_str:<5} | {rr_str:<5} |"
        )

    return "\n".join(table_lines) + "\n"

def extract_symbols(text: str) -> List[str]:
    """Extract stock symbols from text."""
    symbols = re.findall(r'\b[A-Z]{2,5}\b', text)
    exclude = {'THE', 'AND', 'FOR', 'BUY', 'SELL', 'LONG', 'SHORT', 'AT', 'TO', 'BE', 
              'IS', 'ON', 'IN', 'WITH', 'TERM', 'OUTLOOK', 'GOOD', 'ALSO', 'WATCHING'}
    include_futures = {'ES', 'NQ', 'RTY', 'YM', 'SPX', 'SPY', 'QQQ', 'IWM', 'DIA'}
    return [s for s in symbols if s not in exclude or s in include_futures]

def extract_levels(text: str) -> List[float]:
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
    # --- Enhanced Mancini framework fields (your analysis framework) ---
    tier_level: Optional[int] = None          # 0, 1, 4 per Mancini tiers
    acceptance_pattern: Optional[str] = None  # BACKTEST_RETURN, RIP_SELLOFF_RECOVER, etc.
    volatility_context: Optional[str] = None  # HIGH, MODERATE, LOW
    flush_target: Optional[float] = None      # Specific FB flush target level
    test_count: Optional[int] = None          # Number of times level tested
    mancini_confidence: Optional[str] = None  # HIGH, MEDIUM, LOW based on setup quality
    status: TradeStatus = TradeStatus.WAITING  # Simplified status handling
    triggered_at: Optional[float] = None
    current_price: Optional[float] = None

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
        if self.entry > 0 and self.qty > 0:
            return (self.pnl / (self.entry * self.qty)) * 100
        return 0.0

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
    
    # Enhanced reporting fields
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

# === ANALYZER CLASSES ===

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

class ManciniContentFilter:
    """Smart content filtering for Mancini newsletters"""

    # Section boundary patterns
    OPENING_SECTION_ENDS = [
        r"Trade Recap",
        r"Education",
        r"How My Trailing Stop",
        r"The Run Down on",
        r"Before getting into",
        r"This section is intended",
        r"NOTE: The purpose of this trade recap",
    ]

    TRADE_PLAN_STARTS = [
        r"Trade Plan \\w+",  # e.g. Trade Plan Thursday
        r"Trade plan for tomorrow",
        r"For tomorrow",
        r"Tomorrow's plan",
        r"Trade Plan for \\w+",
    ]

    TRADE_PLAN_ENDS = [
        r"As always no crystal balls",
        r"no guessing, no predicting",
        r"Reacting level to level",
    ]

    # New patterns for bull/bear/tomorrow sections
    BULL_SECTION_STARTS = [
        r"Bull case tomorrow:",
        r"Bull case Monday:",
        r"Bull case:",
        r"Bullish case:",
        r"Bullish tomorrow:",
        r"Bullish case tomorrow:",
    ]

    BEAR_SECTION_STARTS = [
        r"Bear case tomorrow:",
        r"Bear case Monday:",
        r"Bear case:",
        r"Bearish case:",
        r"Bearish tomorrow:",
        r"Bearish case tomorrow:",
    ]

    SUMMARY_SECTION_STARTS = [
        r"In summary for tomorrow:",
        r"In summary:",
        r"Summary for tomorrow:",
        r"Tomorrow's summary:",
        r"Key points for tomorrow:",
    ]

    def filter_content(self, content: str) -> Dict[str, str]:
        """Return dict with filtered sections: opening_context, trade_plan, current_position, overnight_action, bull_case, bear_case, summary"""
        lines = content.split("\n")

        def _extract_section(start_cond, end_cond=None, limit=None):
            """Generic helper to extract a block between regex markers."""
            capturing = False
            collected = []
            for line in lines:
                if not capturing and start_cond(line):
                    capturing = True
                if capturing:
                    if end_cond and end_cond(line):
                        break
                    collected.append(line)
                    if limit and len(collected) >= limit:
                        break
            return "\n".join(collected)

        # Opening context (from start until one of the OPENING_SECTION_ENDS)
        opening_context = []
        for line in lines:
            if any(re.search(p, line, re.IGNORECASE) for p in self.OPENING_SECTION_ENDS):
                break
            opening_context.append(line)
        opening_context = "\n".join(opening_context[:50])  # Limit size

        # Trade plan section
        def tp_start(line):
            return any(re.search(p, line, re.IGNORECASE) for p in self.TRADE_PLAN_STARTS)

        def tp_end(line):
            return any(re.search(p, line, re.IGNORECASE) for p in self.TRADE_PLAN_ENDS)

        trade_plan = _extract_section(tp_start, tp_end)

        # Current position lines (grab scattered)
        current_position_lines = [
            ln for ln in lines if any(
                phrase in ln.lower()
                for phrase in [
                    "holding my",
                    "current position",
                    "i am holding",
                    "still holding",
                    "runner",
                    "10% long",
                    "10% short",
                ]
            )
        ]
        current_position = "\n".join(current_position_lines)

        # Overnight action summary (first 10 lines mentioning overnight etc.)
        overnight_lines = [
            ln
            for ln in lines
            if any(
                phrase in ln.lower()
                for phrase in [
                    "overnight",
                    "evening",
                    "morning",
                    "we opened",
                    "heading into today",
                    "last night",
                    "this morning",
                ]
            )
        ]
        overnight_action = "\n".join(overnight_lines[:10])

        # Extract bull/bear/summary sections
        bull_case = self._extract_bull_bear_summary_sections(lines, self.BULL_SECTION_STARTS)
        bear_case = self._extract_bull_bear_summary_sections(lines, self.BEAR_SECTION_STARTS)
        summary = self._extract_bull_bear_summary_sections(lines, self.SUMMARY_SECTION_STARTS)

        return {
            "opening_context": opening_context,
            "trade_plan": trade_plan,
            "current_position": current_position,
            "overnight_action": overnight_action,
            "bull_case": bull_case,
            "bear_case": bear_case,
            "summary": summary,
        }

    def _extract_bull_bear_summary_sections(self, lines: List[str], start_patterns: List[str]) -> str:
        """Extract content from bull/bear/summary sections with flexible heading detection."""
        section_lines = []
        capturing = False
        
        for line in lines:
            # Check if this line starts a new section
            if not capturing and any(re.search(p, line, re.IGNORECASE) for p in start_patterns):
                capturing = True
                continue
                
            # If we're capturing and hit another section start, stop
            if capturing and any(
                re.search(p, line, re.IGNORECASE) 
                for p in self.BULL_SECTION_STARTS + self.BEAR_SECTION_STARTS + self.SUMMARY_SECTION_STARTS
            ):
                break
                
            # If we're capturing, add the line
            if capturing:
                section_lines.append(line)
                
        return "\n".join(section_lines)

class EnhancedManciniAnalyzer:
    """Framework-based Mancini analysis"""

    TIER_SCORES = {
        0: 0.85,  # Failed Breakdown
        1: 0.70,  # Level Reclaim
        4: 0.40,  # Breakdown short (rarely used)
    }

    FAILED_BREAKDOWN_PATTERNS = [
        r"failed breakdown of (\d+)",
        r"fb of (\d+)", 
        r"flush (\d+).*recover",
        r"lose (\d+).*reclaim",
        r"breakdown of (\d+).*(?:backtest|acceptance)",
        r"(\d+).*flush.*acceptance",
    ]

    LEVEL_RECLAIM_PATTERNS = [
        r"reclaim of (\d+)",
        r"recovery of (\d+)", 
        r"back above (\d+)",
        r"acceptance.*(\d+)",
        r"(\d+).*reclaimed.*(\d+)x",  # Capture test count
        r"tested (\d+) (\d+)x",       # Level + test count
    ]

    # Pattern recognition for acceptance types
    ACCEPTANCE_PATTERNS = {
        r"backtest.*return": "BACKTEST_RETURN",
        r"rip.*selloff.*recover": "RIP_SELLOFF_RECOVER", 
        r"acceptance.*hold": "ACCEPTANCE_HOLD",
        r"flush.*acceptance": "FLUSH_ACCEPTANCE",
        r"overnight.*acceptance": "OVERNIGHT_ACCEPTANCE",
    }

    # New patterns for tomorrow's trade ideas
    TOMORROW_LEVEL_PATTERNS = [
        r"(\d+).*support",
        r"(\d+).*resistance",
        r"(\d+).*level",
        r"(\d+).*target",
        r"(\d+).*entry",
        r"(\d+).*stop",
    ]

    # --- Pre-compiled regex sets for performance ---
    FAILED_BREAKDOWN_RE = [re.compile(p, re.IGNORECASE) for p in FAILED_BREAKDOWN_PATTERNS]
    LEVEL_RECLAIM_RE = [re.compile(p, re.IGNORECASE) for p in LEVEL_RECLAIM_PATTERNS]
    ACCEPTANCE_RE = {re.compile(p, re.IGNORECASE): label for p, label in ACCEPTANCE_PATTERNS.items()}
    TOMORROW_LEVEL_RE = [re.compile(p, re.IGNORECASE) for p in TOMORROW_LEVEL_PATTERNS]

    def analyze(self, filtered_sections: Dict[str, str]) -> Dict[str, any]:
        """Return structured analysis from filtered newsletter content."""
        combined_text = f"{filtered_sections['opening_context']}\n{filtered_sections['trade_plan']}"

        market_context = self._extract_market_context(combined_text)
        setups = self._extract_setups(combined_text)
        levels = self._extract_levels(combined_text)

        # Extract tomorrow's trade ideas from bull/bear/summary sections
        tomorrow_setups = self._extract_tomorrow_setups(
            filtered_sections.get("bull_case", ""),
            filtered_sections.get("bear_case", ""),
            filtered_sections.get("summary", "")
        )

        return {
            "market_context": market_context,
            "setups": setups,
            "levels": levels,
            "current_position": filtered_sections["current_position"],
            "overnight_action": filtered_sections["overnight_action"],
            "tomorrow_setups": tomorrow_setups,
        }

    def _extract_tomorrow_setups(self, bull_case: str, bear_case: str, summary: str) -> List[Dict[str, any]]:
        """Extract trade setups from tomorrow's sections."""
        setups = []
        
        # Process bull case
        if bull_case:
            bull_levels = self._extract_levels_from_section(bull_case)
            for level in bull_levels:
                setups.append({
                    "tier": 0,  # High conviction for tomorrow's explicit calls
                    "type": "bull_case",
                    "level": level,
                    "direction": "long",
                    "context": bull_case,
                    "score": 0.90,  # High score for explicit tomorrow calls
                    "mancini_confidence": "HIGH",
                    "source_section": "bull_case"
                })
        
        # Process bear case
        if bear_case:
            bear_levels = self._extract_levels_from_section(bear_case)
            for level in bear_levels:
                setups.append({
                    "tier": 0,  # High conviction for tomorrow's explicit calls
                    "type": "bear_case",
                    "level": level,
                    "direction": "short",
                    "context": bear_case,
                    "score": 0.90,  # High score for explicit tomorrow calls
                    "mancini_confidence": "HIGH",
                    "source_section": "bear_case"
                })
        
        # Process summary for additional context
        if summary:
            summary_levels = self._extract_levels_from_section(summary)
            for level in summary_levels:
                # Only add if not already captured in bull/bear cases
                if not any(s["level"] == level for s in setups):
                    setups.append({
                        "tier": 1,  # Medium conviction for summary mentions
                        "type": "summary_level",
                        "level": level,
                        "direction": "long",  # Default to long, will be adjusted based on context
                        "context": summary,
                        "score": 0.70,
                        "mancini_confidence": "MEDIUM",
                        "source_section": "summary"
                    })
        
        return setups

    def _extract_levels_from_section(self, text: str) -> List[float]:
        """Extract price levels from a section using tomorrow-specific patterns."""
        levels = set()
        
        # Extract using tomorrow-specific patterns
        for pat in self.TOMORROW_LEVEL_RE:
            for m in pat.finditer(text):
                try:
                    level = float(m.group(1))
                    if 3000 <= level <= 99999:  # Valid ES level range
                        levels.add(level)
                except (ValueError, IndexError):
                    continue
        
        return sorted(list(levels))

    def _extract_market_context(self, text: str) -> Dict[str, str]:
        tl = text.lower()

        regime = "RANGE_BOUND"
        if "buy dips" in tl or "buy-dips" in tl:
            regime = "BUY_DIPS"
        elif "sell rips" in tl or "sell-rips" in tl:
            regime = "SELL_RIPS"

        mode = "Mode2"
        if "mode 1" in tl or "trend day" in tl:
            mode = "Mode1"

        volatility = "MODERATE"
        if "high vol" in tl or "volatile" in tl:
            volatility = "HIGH"
        elif "low vol" in tl or "slow" in tl:
            volatility = "LOW"

        return {"regime": regime, "mode": mode, "volatility": volatility}

    def _extract_setups(self, text: str) -> List[Dict[str, any]]:
        setups: List[Dict[str, any]] = []

        # Tier 0 – Failed Breakdowns (Enhanced)
        for pat in self.FAILED_BREAKDOWN_RE:
            for m in pat.finditer(text):
                lvl = float(m.group(1))
                
                # Detect acceptance pattern
                acceptance_pattern = "STANDARD_FB"
                for acc_pat, acc_type in self.ACCEPTANCE_RE.items():
                    if acc_pat.search(m.group(0)):
                        acceptance_pattern = acc_type
                        break
                
                # Extract flush target if mentioned
                flush_target = None
                flush_match = re.search(r"flush\s+(\d+)", m.group(0))
                if flush_match:
                    flush_target = float(flush_match.group(1))
                
                setups.append({
                    "tier": 0,
                    "type": "failed_breakdown", 
                    "level": lvl,
                    "direction": "long",
                    "context": m.group(0),
                    "score": self.TIER_SCORES[0],
                    "acceptance_pattern": acceptance_pattern,
                    "flush_target": flush_target,
                    "mancini_confidence": "HIGH" if "backtest" in m.group(0).lower() else "MEDIUM"
                })

        # Tier 1 – Level Reclaims (Enhanced)
        for pat in self.LEVEL_RECLAIM_RE:
            for m in pat.finditer(text):
                lvl = float(m.group(1))
                
                # Extract test count if available
                test_count = None
                test_match = re.search(r"(\d+)x", m.group(0))
                if test_match:
                    test_count = int(test_match.group(1))
                
                # Confidence based on test count
                confidence = "HIGH" if test_count and test_count >= 3 else "MEDIUM"
                
                setups.append({
                    "tier": 1,
                    "type": "level_reclaim",
                    "level": lvl, 
                    "direction": "long",
                    "context": m.group(0),
                    "score": self.TIER_SCORES[1],
                    "test_count": test_count,
                    "mancini_confidence": confidence
                })

        return setups

    def _extract_levels(self, text: str) -> List[float]:
        # Extract 4- or 5-digit ES levels (future-proof 3,000-99,999 range)
        nums = re.findall(r"\b(\d{4,5})\b", text)
        lvls = sorted({float(n) for n in nums if 3000 <= float(n) <= 99999})
        return lvls

# === MAIN INTENT TRADER ===

class IntentTrader:
    """Complete PFEMRC trading assistant with source-based scoring."""
    
    def __init__(self):
        self.context = TradingContext()
        self.handlers = self._register_handlers()
        
    def __str__(self):
        """Display enhanced startup screen showcasing PFEMRC workflow."""
        return f"""
╔══════════════════════════════════════════════════════════════╗
║                    INTENT TRADER v0.4.3                     ║
║              Plan-Focus-Execute-Manage-Review-Coach          ║
╠══════════════════════════════════════════════════════════════╣
║ Phase: {self.context.phase:<8} │ Mode: {self.context.mode:<6} │ P&L: ${self.context.realized_pnl:>8.2f} ║
║ Ideas: {len(self.context.ideas):<8} │ Positions: {len(self.context.positions):<3} │ Stops: {self.context.stops_hit}/3     ║
╚══════════════════════════════════════════════════════════════╝

🎯 PLAN PHASE - Multi-Source Analysis Framework:

📊 Morning Analysis:
   • "analyze dp [paste DP morning call transcript]"
   • "analyze mancini [paste Mancini newsletter]"

🔍 Advanced Setups:
   • "add ES mancini failed breakdown 6024 stop 6010 target 6040 6055"  
   • "add AAPL dp focus trade entry 225 stop 223 target 235 245"

⚡ Quick Commands:
   • "create plan" → Unified trading plan with conviction scoring
   • "show plan" → Live trading table with all setups
   • "execute plan ES" → Trigger when ES hits Failed Breakdown
   • "lock 75" → Mancini 75% profit lock protocol
   • "coach" → Behavioral pattern analysis

💡 Say it naturally: "Buy 100 AAPL at 225" or "ES looks ready for FB"

Ready for your first analysis. What's your move?
"""
    
    def _register_handlers(self) -> Dict[str, callable]:
        """Register all command handlers."""
        return {
            "analyze dp": self.handle_analyze_dp,
            "analyze mancini": self.handle_analyze_mancini,
            "create plan": self.handle_create_plan,
            "show plan": self.handle_show_plan,
            "execute": self.handle_execute,
            "positions": self.handle_positions,
            "help": self.handle_help,
            "add trade": self.handle_add_trade,
            "update prices": self.handle_update_prices,
            "execute from plan": self.handle_execute_from_plan,
            "exit": self.handle_exit,
            "move stop": self.handle_move_stop,
            "lock profits": self.handle_lock_profits,
            "save": self.handle_save,
            "load": self.handle_load,
            "invalidate": self.handle_invalidate,
            "journal": self.handle_journal,
            "chart": self.handle_chart,
            "coach": self.handle_coach,
            "review": self.handle_review,
            "behavioral check": self.handle_behavioral_check,
            "log moderator": self.handle_log_moderator,
            "size position": self.handle_size_position,
            "show offset": self.handle_show_offset,
            "set offset": self.handle_set_offset,  # New handler
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
        levels = extract_levels(message)
        symbols = extract_symbols(message)
        
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
            symbols_in_line = extract_symbols(line)
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
                    prices = extract_levels(line)
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
                response += format_plan_table(exceptional)
                    
            if high:
                response += "\nHIGH (0.70-0.89) - Full Size:\n"
                response += format_plan_table(high)
                    
            if medium:
                response += "\nMEDIUM (0.50-0.69) - Half Size:\n"
                response += format_plan_table(medium)
                    
        response += "\n-> Next: 'analyze mancini' or 'create plan'"
        return response
    
    def handle_analyze_mancini(self, message: str) -> str:
        """Enhanced Mancini analysis with smart content filtering and tier-based scoring."""

        # Phase 1: Filter to actionable content
        content_filter = ManciniContentFilter()
        filtered = content_filter.filter_content(message)

        # Phase 2: Structured analysis with enhanced framework
        analyzer = EnhancedManciniAnalyzer()
        analysis = analyzer.analyze(filtered)

        # Update context mode from market context
        self.context.mode = analysis["market_context"].get("mode", "Mode2")

        # Convert setups to TradeIdea objects with enhanced framework
        for setup in analysis["setups"]:
            # Skip duplicate idea for same level & tier
            existing = next((i for i in self.context.ideas if i.source == "mancini" and getattr(i, "tier_level", None) == setup["tier"] and i.entry == setup["level"]), None)
            if existing:
                continue

            # Map tier to familiar label for legacy compatibility
            tier_label_map = {0: "FB", 1: "Reclaim", 4: "Breakdown"}
            label = tier_label_map.get(setup["tier"], f"Tier {setup['tier']}")

            idea = TradeIdea(
                ticker="ES",
                source="mancini",
                score=ConvictionScore(setup["score"], "mancini", label),
                entry=setup["level"],
                notes=setup["context"],
                tier_level=setup["tier"],
                volatility_context=analysis["market_context"].get("volatility"),
                # Enhanced framework fields
                acceptance_pattern=setup.get("acceptance_pattern"),
                flush_target=setup.get("flush_target"),
                test_count=setup.get("test_count"),
                mancini_confidence=setup.get("mancini_confidence", "MEDIUM"),
            )
            self.context.ideas.append(idea)

        # Add tomorrow's trade ideas
        for setup in analysis.get("tomorrow_setups", []):
            # Skip if we already have this level
            existing = next((i for i in self.context.ideas if i.source == "mancini" and i.entry == setup["level"]), None)
            if existing:
                continue

            # Create label based on source section
            label = setup["type"].upper()
            if setup["type"] == "bull_case":
                label = "BULL_CALL"
            elif setup["type"] == "bear_case":
                label = "BEAR_CALL"

            idea = TradeIdea(
                ticker="ES",
                source="mancini",
                score=ConvictionScore(setup["score"], "mancini", label),
                entry=setup["level"],
                notes=setup["context"],
                tier_level=setup["tier"],
                type=setup["direction"],
                volatility_context=analysis["market_context"].get("volatility"),
                mancini_confidence=setup["mancini_confidence"],
            )
            self.context.ideas.append(idea)

        # Store full analysis for later reference
        self.context.mancini_analysis = analysis

        # Build response grouped by tiers
        response_lines = ["=== MANCINI ANALYSIS (FILTERED) ==="]
        mc = analysis["market_context"]
        response_lines.append(f"Market Regime: {mc['regime']} | Mode: {mc['mode']} | Vol: {mc['volatility']}")

        # Tomorrow's Trade Ideas (New Section)
        tomorrow_setups = analysis.get("tomorrow_setups", [])
        if tomorrow_setups:
            response_lines.append("\n=== TOMORROW'S TRADE IDEAS ===")
            
            # Group by type
            bull_calls = [s for s in tomorrow_setups if s["type"] == "bull_case"]
            bear_calls = [s for s in tomorrow_setups if s["type"] == "bear_case"]
            summary_levels = [s for s in tomorrow_setups if s["type"] == "summary_level"]
            
            if bull_calls:
                response_lines.append("\nBULL CASE:")
                for s in bull_calls:
                    response_lines.append(f"• ES {s['level']:.0f} LONG (score {s['score']:.2f})")
            
            if bear_calls:
                response_lines.append("\nBEAR CASE:")
                for s in bear_calls:
                    response_lines.append(f"• ES {s['level']:.0f} SHORT (score {s['score']:.2f})")
            
            if summary_levels:
                response_lines.append("\nKEY LEVELS:")
                for s in summary_levels:
                    response_lines.append(f"• ES {s['level']:.0f} (score {s['score']:.2f})")

        # Regular Tier Analysis
        tier0 = [s for s in analysis["setups"] if s["tier"] == 0]
        tier1 = [s for s in analysis["setups"] if s["tier"] == 1]
        tier4 = [s for s in analysis["setups"] if s["tier"] == 4]

        if tier0:
            response_lines.append("\nTIER 0: FAILED BREAKDOWNS (Core Edge)")
            for s in tier0:
                response_lines.append(f"• ES {s['level']:.0f} FB (score {s['score']:.2f})")

        if tier1:
            response_lines.append("\nTIER 1: LEVEL RECLAIMS")
            for s in tier1:
                response_lines.append(f"• ES {s['level']:.0f} Reclaim (score {s['score']:.2f})")

        if tier4:
            response_lines.append("\nTIER 4: BREAKDOWN SHORT")
            for s in tier4:
                response_lines.append(f"• ES {s['level']:.0f} Breakdown (score {s['score']:.2f})")

        if analysis.get("current_position"):
            response_lines.append("\nCURRENT POSITION SUMMARY:")
            response_lines.append(analysis["current_position"][:200] + ("..." if len(analysis["current_position"]) > 200 else ""))

        response_lines.append("\n-> Next: 'create plan' to merge with DP analysis")

        return "\n".join(response_lines)
    
    def handle_create_plan(self, message: str) -> str:
        """Create or refresh the daily trading plan as a single unified table."""
        # Simply render everything currently in context.ideas using the new unified table.
        response = "=== DAILY TRADING PLAN ===\n"
        response += f"Phase: PLAN -> FOCUS\n"
        response += f"Market Mode: {self.context.mode}\n\n"

        response += format_plan_table(self.context.ideas)

        # Generic execution rules (no longer source-specific for layout)
        response += "\nEXECUTION RULES:\n"
        response += "* Size positions using conviction score (see 'size position <TICKER>')\n"
        response += "* Confirm price is at ENTRY before executing a trade\n"
        response += "* Respect STOP levels – no moving without a plan\n"

        self.context.phase = "FOCUS"
        response += "\n-> Phase updated to FOCUS"
        response += "\n-> Use 'show plan' to see or filter the table\n"

        return response
    
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
        
        # Use central table helper
        response += format_plan_table(ideas_to_show)
        
        # Add action hints
        response += "\nACTIONS:\n"
        response += "• 'update prices AAPL 225 TSLA 420' - Update current prices\n"
        response += "• 'execute plan AAPL' - Execute when price hits entry\n"
        response += "• 'invalidate AAPL' - Remove setup that's no longer valid\n"
        response += "• 'waiting/active/done' - Filter by status\n"
        
        return response

    # === CORE HANDLERS (kept readable and clear) ===
    
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

        if not price_str:
            return "Price required. Specify like 'buy AAPL @ 225.50'"

        try:
            price = float(price_str)
            if price <= 0:
                return "❌ Price must be positive"
        except ValueError:
            return "❌ Invalid price format"
        
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
            entry=price,
            current=price,
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
    
    def handle_help(self, message: str) -> str:
        """Show help information."""
        return """
=== INTENT TRADER v0.4.3 HELP ===

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

    # === REMAINING HANDLERS (simplified but functional) ===
    
    def handle_add_trade(self, message: str) -> str:
        """Add a new trade idea with proper source routing and full parameters."""
        parts = message.split()
        if len(parts) < 2:
            return "Usage: add AAPL dp love this setup entry 225 stop 223 target 230 235"
            
        ticker = parts[1].upper()
        
        # Parse parameters using shared helper
        entry = stop = target1 = target2 = None
        msg_lower = message.lower()
        
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
                    # Check for existing idea
                    existing = next((i for i in self.context.ideas if i.ticker == ticker and i.source == "dp"), None)
                    if existing:
                        existing.score = ConvictionScore(score, "dp", label)
                        existing.entry = entry or existing.entry
                        existing.stop = stop or existing.stop
                        existing.target1 = target1 or existing.target1
                        existing.target2 = target2 or existing.target2
                        return f"✅ Updated {ticker} from DP: {label} ({score:.2f})"
                    
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
                    # Check for existing idea
                    existing = next((i for i in self.context.ideas if i.ticker == ticker and i.source == "mancini"), None)
                    if existing:
                        existing.score = ConvictionScore(score, "mancini", label)
                        existing.entry = entry or existing.entry
                        existing.stop = stop or existing.stop
                        existing.target1 = target1 or existing.target1
                        existing.target2 = target2 or existing.target2
                        return f"✅ Updated {ticker} from Mancini: {label} ({score:.2f})"
                    
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

    # === REMAINING HANDLERS (keeping readability over compression) ===
    
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
                if price <= 0:
                    return f"❌ Price for {ticker} must be positive"
                
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
        
        # Validate entry price exists
        if not idea.entry:
            return f"❌ No entry price set for {ticker}. Add entry price first\n\n" + self.handle_show_plan("")
            
        # Check if at entry
        if idea.type == "long" and idea.current_price > idea.entry * 1.01:
            return f"❌ {ticker} above entry ${idea.entry:.2f} (current: ${idea.current_price:.2f})\n\n" + self.handle_show_plan("")
        elif idea.type == "short" and idea.current_price < idea.entry * 0.99:
            return f"❌ {ticker} below entry ${idea.entry:.2f} (current: ${idea.current_price:.2f})\n\n" + self.handle_show_plan("")
        
        # Risk check
        if self.context.stops_hit >= MAX_STOPS_ALLOWED:
            return f"❌ Cannot execute: {MAX_STOPS_ALLOWED} stops already hit. Maximum risk reached.\n\n" + self.handle_show_plan("")
        
        # Calculate position size based on conviction (using shared helper)
        qty = size_for_score(idea.score.score, self.context.mode)
        
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

    # Keep remaining handlers clear and simple
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
        symbols = extract_symbols(message)
        if not symbols:
            return "Specify: 'exit AAPL' or 'exit all'"
            
        ticker = symbols[0]
        pos = next((p for p in self.context.positions if p.ticker == ticker), None)
        
        if not pos:
            return f"No position in {ticker}"
            
        # Update price if provided
        levels = extract_levels(message)
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

    # Additional handlers kept simple for readability
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
        symbols = extract_symbols(message)
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
        
        # Update position or remove if fully closed
        if remaining_qty <= 0:
            self.context.positions.remove(pos)
            self.context.realized_pnl += pos.pnl
            response = f"✅ LOCKED 100% PROFITS: {ticker}\n"
            response += f"Closed entire position for ${pos.pnl:+.2f}\n"
        else:
            pos.qty = remaining_qty
            self.context.realized_pnl += exit_pnl
            response = f"✅ LOCKED 75% PROFITS: {ticker}\n"
            response += f"Sold {exit_qty} units for ${exit_pnl:+.2f}\n"
            response += f"Runner: {remaining_qty} units remain\n"
            response += "📋 Trail stop on runner to next level"
        
        return response

    # Minimal implementations for completeness
    def handle_save(self, message: str) -> str:
        """Save context to JSON."""
        filename = f"trader_{datetime.now().strftime('%Y%m%d_%H%M')}.json"
        data = asdict(self.context)
        # Convert Enums to their value for JSON serialization
        def _enum_converter(o):
            return o.value if isinstance(o, Enum) else str(o)

        json_str = json.dumps(data, indent=2, default=_enum_converter)
        
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
            
            # Clean / reconstruct ideas (support legacy _status key)
            ideas_raw = data.get('ideas', [])
            cleaned_ideas = []
            for idea_dict in ideas_raw:
                idea_dict.pop('_status', None)  # legacy cleanup
                cleaned_ideas.append(idea_dict)

            # Reconstruct ideas with proper type conversion
            self.context.ideas = []
            for idea_dict in cleaned_ideas:
                # Handle ConvictionScore reconstruction
                if isinstance(idea_dict.get('score'), dict):
                    idea_dict['score'] = ConvictionScore(**idea_dict['score'])
                
                # Handle TradeStatus reconstruction
                if isinstance(idea_dict.get('status'), str):
                    idea_dict['status'] = TradeStatus(idea_dict['status'])
                
                # Create TradeIdea with proper types
                try:
                    idea = TradeIdea(**idea_dict)
                    self.context.ideas.append(idea)
                except Exception as e:
                    # Skip malformed ideas rather than crash
                    continue
                    
            self.context.positions = [Position(**pos) for pos in data.get('positions', [])]
            
            return f"""✅ SESSION RESTORED

Phase: {self.context.phase}
Positions: {len(self.context.positions)}
Ideas: {len(self.context.ideas)}
P&L: ${self.context.realized_pnl:.2f}

Ready to continue trading!"""
            
        except Exception as e:
            return f"❌ Load failed: {str(e)}"

    # Simple implementations for remaining handlers
    def handle_invalidate(self, message: str) -> str:
        """Invalidate a setup that's no longer valid."""
        symbols = extract_symbols(message)
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

    def handle_chart(self, message: str) -> str:
        """Decode chart visuals and link to actionable trades."""
        msg = message.lower()
        ticker = next((s for s in extract_symbols(message) if s not in ['FB', 'ES', 'YH', 'YL', 'HOD', 'LOD']), None)
        
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

    def handle_behavioral_check(self, message: str) -> str:
        """Real-time behavioral check."""
        alert = self._check_behavioral_patterns()
        if alert:
            return alert
        return "✅ No behavioral issues detected"

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

    # Additional simple handlers
    def handle_size_position(self, message: str) -> str:
        """Calculate position size based on source rules."""
        symbols = extract_symbols(message)
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
        
        # Use shared sizing logic
        base_size = size_for_score(best_idea.score.score, self.context.mode)
        response += f"Recommended Size: {base_size} shares\n"
        
        if best_idea.source == "dp":
            if best_idea.score.score >= 0.90:
                response += "Rationale: FOCUS TRADE - Full size+\n"
            elif best_idea.score.score >= 0.70:
                response += "Rationale: HIGH CONVICTION - Full size\n"
            elif best_idea.score.score >= 0.50:
                response += "Rationale: MEDIUM CONVICTION - Half size\n"
            else:
                response += "Rationale: LOW CONVICTION - Quarter size\n"
        else:  # mancini
            if best_idea.score.label == "FB":
                response += "Rationale: FAILED BREAKDOWN - Core edge\n"
            elif best_idea.score.score >= 0.70:
                response += "Rationale: STRONG SETUP - Full size\n"
            else:
                response += "Rationale: WEAKER SETUP - Reduced size\n"
                
        # Mode adjustment
        if self.context.mode == "Mode2":
            response += "\n⚠️ Mode 2 Market: Size reduced 25% automatically"
            
        return response

    def handle_unknown(self, message: str) -> str:
        """Handle unknown messages."""
        return f"""❓ I didn't understand that. Say 'help' to see what I can do.

You're currently in {self.context.phase} phase.

Try commands like:
• analyze dp [text]
• show plan
• buy AAPL
• positions"""

    def handle_show_offset(self, message: str) -> str:
        """Show the current ES-SPX offset and its status."""
        offset = get_es_spx_offset()
        status = "manual" if manual_override_offset is not None else "auto"
        
        response = f"📊 ES-SPX Offset: {offset} points ({status})\n"
        
        if status == "auto":
            # Show next expiry info
            now = datetime.now()
            expiry_dates = [
                datetime(now.year, 3, 15),
                datetime(now.year, 6, 21),
                datetime(now.year, 9, 20),
                datetime(now.year, 12, 20),
            ]
            
            next_expiry = None
            for expiry in expiry_dates:
                if expiry > now:
                    next_expiry = expiry
                    break
                    
            if next_expiry:
                days_to_expiry = (next_expiry - now).days
                response += f"Next expiry: {next_expiry.strftime('%Y-%m-%d')} ({days_to_expiry} days)\n"
                response += f"Offset will decay to ~5 points by expiry\n"
        
        return response

    def handle_set_offset(self, message: str) -> str:
        """Set a manual override for the ES-SPX offset.
        
        Usage:
            set offset 12    # Set manual offset to 12 points
            set offset auto  # Reset to automatic calculation
        """
        global manual_override_offset
        
        parts = message.lower().split()
        if len(parts) < 3:
            return "❌ Usage: set offset <points> or set offset auto"
            
        value = parts[2]
        if value == "auto":
            manual_override_offset = None
            return "✅ ES-SPX offset reset to automatic calculation"
            
        try:
            offset = int(value)
            if offset < 0 or offset > 50:
                return "❌ Offset must be between 0 and 50 points"
            manual_override_offset = offset
            return f"✅ ES-SPX offset set to {offset} points (manual)"
        except ValueError:
            return "❌ Invalid offset value. Use a number or 'auto'"

# === GLOBAL FUNCTIONS ===

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
        trader = initialize_trader()
    return trader.process(message)

# Example usage for AI assistants
def demo():
    """Demonstration of how to use IntentTrader with an AI Assistant."""
    
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
|  Intent Trader v0.4.3 - AI Assistant           |
|                                                |
|  This version is designed for AI assistants.   |
|  For interactive use, import and use:          |
|  - initialize_trader() to start                |
|  - say(message) to execute commands            |
|  - demo() to see examples                      |
+------------------------------------------------+
        """)