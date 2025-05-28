# intent_trader.py v0.6.0 — POSITIONS HANDLER FULLY UPGRADED

from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional
import json
import datetime

@dataclass
class ConvictionScore:
    score: float
    source: str
    label: str

@dataclass
class TradeIntent:
    ticker: str
    action: str
    source: str
    score: ConvictionScore
    entry: Optional[float] = None
    stop: Optional[float] = None
    target: Optional[float] = None
    size: Optional[str] = None
    outcome: Optional[str] = None
    pnl: Optional[float] = None
    notes: Optional[str] = None

@dataclass
class TradePlan:
    phase: str = "PLAN"
    intents: List[TradeIntent] = field(default_factory=list)
    positions: Dict[str, str] = field(default_factory=dict)
    pnl: float = 0.0
    journal: List[str] = field(default_factory=list)
    mode: Optional[str] = None

DP_LANGUAGE_MAP = [
    ("focus trade", 0.95, "Exceptional"),
    ("get aggressive", 0.95, "Exceptional"),
    ("love this", 0.93, "Exceptional"),
    ("definitely want", 0.85, "High"),
    ("strong conviction", 0.80, "High"),
    ("really like", 0.75, "High"),
    ("i'm a buyer", 0.65, "Medium"),
    ("worth owning", 0.60, "Medium"),
    ("decent setup", 0.55, "Medium"),
    ("worth watching", 0.45, "Low"),
    ("might work", 0.40, "Low"),
    ("not excited", 0.20, "Avoid"),
    ("stay away", 0.10, "Avoid"),
]

MANCINI_SETUP_MAP = {
    "failed breakdown": (0.90, "FB"),
    "level reclaim": (0.75, "Reclaim"),
    "support test": (0.65, "Support"),
    "mode 2 range": (0.50, "Trap"),
    "against mode": (0.30, "Avoid"),
}

class IntentTrader:
    def __init__(self):
        self.context = TradePlan()
        self.handlers = self._register_intents()

    def _register_intents(self):
        return {
            "help": self.handle_help,
            "reset": self.handle_reset,
            "context": self.handle_context,
            "save": self.handle_save,
            "load": self.handle_load,
            "journal": self.handle_journal,
            "analyze dp": self.handle_analyze_dp,
            "analyze mancini": self.handle_analyze_mancini,
            "create plan": self.handle_create_plan,
            "focus trades": self.handle_focus_trades,
            "add": self.handle_add_trade,
            "positions": self.handle_positions,
            "review": self.handle_review,
            "coach": self.handle_coach,
            "show plan": self.handle_show_plan,
            "tag outcome": self.handle_tag_outcome,
            "filter plan": self.handle_filter_plan,
            "summarize performance": self.handle_summarize_performance,
        }

    def route(self, message: str) -> str:
        for intent, handler in self.handlers.items():
            if intent in message.lower():
                return handler(message)
        return "No matching intent found. Type 'help' for available commands."

    def handle_help(self, msg):
        return "\nCommands: " + ", ".join(self.handlers)

    def handle_reset(self, msg):
        self.context = TradePlan()
        return "Context reset."

    def handle_context(self, msg):
        return json.dumps(asdict(self.context), indent=2)

    def handle_save(self, msg):
        with open("context.json", "w") as f:
            json.dump(asdict(self.context), f)
        return "Context saved."

    def handle_load(self, msg):
        try:
            with open("context.json", "r") as f:
                data = json.load(f)
            self.context = TradePlan(**data)
            return "Context loaded."
        except Exception as e:
            return f"Load failed: {e}"

    def handle_journal(self, msg):
        entry = f"[{datetime.datetime.now().isoformat()}] {msg}"
        self.context.journal.append(entry)
        return f"Journaled: {entry}"

    def handle_positions(self, msg):
        parts = msg.lower().split()
        if len(parts) == 1:
            return json.dumps(self.context.positions, indent=2) or "No positions."
        if len(parts) >= 3:
            action = parts[1]
            ticker = parts[2].upper()
            position_note = " ".join(parts[3:]) if len(parts) > 3 else ""

            if action == "add":
                self.context.positions[ticker] = f"LONG {position_note}".strip()
                return f"✓ Added position for {ticker}"
            elif action == "trim":
                self.context.positions[ticker] = f"TRIMMED {position_note}".strip()
                return f"✓ Trimmed position for {ticker}"
            elif action == "exit":
                self.context.positions.pop(ticker, None)
                return f"✓ Exited position for {ticker}"

        return "Usage: positions [add|trim|exit] TICKER [notes] or just 'positions' to view current."

    def handle_analyze_dp(self, msg):
        lines = msg.lower().split('\n')
        scored = []
        for line in lines:
            for phrase, score, label in DP_LANGUAGE_MAP:
                if phrase in line:
                    scored.append(f"✓ '{line.strip()}' → Score: {score:.2f} ({label})")
                    break
        return "\n".join(scored) if scored else "No conviction language detected."

    def handle_analyze_mancini(self, msg):
        for key, (score, label) in MANCINI_SETUP_MAP.items():
            if key in msg.lower():
                return f"✓ Mancini setup '{key.title()}' → Score: {score:.2f} ({label})"
        return "No valid Mancini setup detected."

    def handle_create_plan(self, msg):
        lines = msg.strip().split("\n")
        count = 0
        for line in lines:
            line = line.strip().lower()
            if not line or line.startswith("#"):
                continue
            parts = line.split()
            ticker = parts[0].upper()
            source = parts[1] if parts[1] in ("dp", "mancini") else "dp"
            text = " ".join(parts[2:])

            if source == "dp":
                for phrase, score, label in DP_LANGUAGE_MAP:
                    if phrase in text:
                        self.context.intents.append(TradeIntent(
                            ticker=ticker,
                            action="add",
                            source="dp",
                            score=ConvictionScore(score, "dp", label)
                        ))
                        count += 1
                        break

            elif source == "mancini":
                for phrase, (score, label) in MANCINI_SETUP_MAP.items():
                    if phrase in text:
                        self.context.intents.append(TradeIntent(
                            ticker=ticker,
                            action="add",
                            source="mancini",
                            score=ConvictionScore(score, "mancini", label)
                        ))
                        count += 1
                        break
        return f"✓ Created trade plan with {count} entries."

    def handle_focus_trades(self, msg):
        output = []
        for t in self.context.intents:
            if (t.source == "dp" and t.score.score >= 0.90) or (t.source == "mancini" and t.score.score >= 0.85):
                output.append(f"{t.ticker} ({t.source}) → {t.score.label} ({t.score.score:.2f})")
        return "\n".join(output) or "No focus trades found."

    def handle_add_trade(self, msg):
        parts = msg.lower().split()
        if len(parts) < 4:
            return "Usage: add [TICKER] [SOURCE] [PHRASE]"
        ticker = parts[1].upper()
        source = parts[2]
        text = " ".join(parts[3:])

        if source == "dp":
            for phrase, score, label in DP_LANGUAGE_MAP:
                if phrase in text:
                    self.context.intents.append(TradeIntent(
                        ticker=ticker,
                        action="add",
                        source="dp",
                        score=ConvictionScore(score, "dp", label)
                    ))
                    return f"✓ Added {ticker} from DP with {label} ({score:.2f})"

        elif source == "mancini":
            for phrase, (score, label) in MANCINI_SETUP_MAP.items():
                if phrase in text:
                    self.context.intents.append(TradeIntent(
                        ticker=ticker,
                        action="add",
                        source="mancini",
                        score=ConvictionScore(score, "mancini", label)
                    ))
                    return f"✓ Added {ticker} from Mancini with {label} ({score:.2f})"

        return "Phrase not recognized for scoring."

    def handle_review(self, msg):
        wins = sum(1 for t in self.context.intents if t.outcome == "win")
        losses = sum(1 for t in self.context.intents if t.outcome == "loss")
        breakevens = sum(1 for t in self.context.intents if t.outcome == "breakeven")
        total_pnl = sum(t.pnl or 0 for t in self.context.intents)
        return f"✓ Wins: {wins}  Losses: {losses}  Breakeven: {breakevens}\nTotal P&L: {total_pnl:.2f}"

    def handle_tag_outcome(self, msg):
        try:
            _, _, ticker, outcome, pnl = msg.split()
            for t in self.context.intents:
                if t.ticker == ticker:
                    t.outcome = outcome
                    t.pnl = float(pnl)
                    journal_entry = f"Tagged {ticker} as {outcome} with P&L {pnl}"
                    self.context.journal.append(f"[{datetime.datetime.now().isoformat()}] {journal_entry}")
                    return f"✓ {journal_entry}"
            return "Trade not found."
        except:
            return "Usage: tag outcome [TICKER] [win/loss/breakeven] [P&L]"

    def handle_show_plan(self, msg):
        return "\n".join(f"{t.ticker}: {t.source} {t.score.label} ({t.score.score:.2f})" for t in self.context.intents) or "No trades in plan."

    def handle_filter_plan(self, msg):
        parts = msg.lower().split()
        if len(parts) < 3:
            return "Usage: filter plan [source] [min_score]"
        source = parts[2]
        min_score = float(parts[3]) if len(parts) > 3 else 0.0
        output = [f"{t.ticker}: {t.score.label} ({t.score.score:.2f})" for t in self.context.intents if t.source == source and t.score.score >= min_score]
        return "\n".join(output) or "No matching trades."

    def handle_summarize_performance(self, msg):
        buckets = {}
        for t in self.context.intents:
            buckets[t.score.label] = buckets.get(t.score.label, 0) + 1
        return "\n".join(f"{k}: {v}" for k, v in buckets.items())

    def handle_coach(self, msg):
        flags = []
        if sum(1 for t in self.context.intents if t.outcome == "loss") >= 3:
            flags.append("⚠️ Possible revenge trading: 3+ consecutive losses")
        if len(self.context.intents) > 10:
            flags.append("⚠️ Overtrading detected")
        return "\n".join(flags) or "No coaching alerts."

if __name__ == "__main__":
    trader = IntentTrader()
    print("Intent Trader v0.6.0 ready.")
    while True:
        user_input = input("\n> ")
        if user_input.lower() in ["quit", "exit"]:
            break
        print(trader.route(user_input))
