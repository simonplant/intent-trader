# intent_trader_v060_final.py — FULL FINAL IMPLEMENTATION

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
    mode: Optional[str] = "LIVE"

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
            "simulate": self.handle_simulate,
        }

    def route(self, message: str) -> str:
        for intent, handler in self.handlers.items():
            if intent in message.lower():
                return handler(message)
        return "No matching intent found. Type 'help' for available commands."

    def handle_help(self, msg): return "\nCommands: " + ", ".join(self.handlers)

    def handle_reset(self, msg):
        self.context = TradePlan()
        return "Context reset."

    def handle_context(self, msg): return json.dumps(asdict(self.context), indent=2)

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
        parts = msg.strip().split()
        if len(parts) == 1:
            return "\n".join(self.context.journal[-5:]) or "Journal is empty."

        cmd = parts[1]
        if cmd == "search" and len(parts) > 2:
            query = " ".join(parts[2:]).lower()
            matches = [j for j in self.context.journal if query in j.lower()]
            return "\n".join(matches) or f"No matches for '{query}'."
        if cmd == "date" and len(parts) > 2:
            date = parts[2]
            filtered = [j for j in self.context.journal if j.startswith(f"[{date}")]
            return "\n".join(filtered) or f"No entries on {date}."

        entry = f"[{datetime.datetime.now().isoformat()}] {' '.join(parts[1:])}"
        self.context.journal.append(entry)
        return f"Journaled: {entry}"

    def handle_positions(self, msg):
        parts = msg.lower().split()
        if len(parts) == 1:
            return json.dumps(self.context.positions, indent=2) or "No positions."
        if len(parts) >= 3:
            action, ticker = parts[1], parts[2].upper()
            note = " ".join(parts[3:]) if len(parts) > 3 else ""
            if action == "add":
                self.context.positions[ticker] = f"LONG {note}".strip()
                return f"✓ Added position for {ticker}"
            elif action == "trim":
                self.context.positions[ticker] = f"TRIMMED {note}".strip()
                return f"✓ Trimmed position for {ticker}"
            elif action == "exit":
                self.context.positions.pop(ticker, None)
                return f"✓ Exited position for {ticker}"
        return "Usage: positions [add|trim|exit] TICKER [notes]"

    def handle_simulate(self, msg):
        sample = "TSLA dp love this\nAAPL mancini support test\nNFLX dp worth watching"
        return self.handle_create_plan(sample)

    def handle_create_plan(self, msg):
        lines = msg.strip().split("\n")
        count = 0
        for line in lines:
            parts = line.lower().strip().split()
            if len(parts) < 3: continue
            ticker, source = parts[0].upper(), parts[1]
            text = " ".join(parts[2:])
            if source == "dp":
                for phrase, score, label in DP_LANGUAGE_MAP:
                    if phrase in text:
                        self.context.intents.append(TradeIntent(ticker, "add", source, ConvictionScore(score, source, label)))
                        count += 1
                        break
            if source == "mancini":
                for phrase, (score, label) in MANCINI_SETUP_MAP.items():
                    if phrase in text:
                        self.context.intents.append(TradeIntent(ticker, "add", source, ConvictionScore(score, source, label)))
                        count += 1
                        break
        return f"✓ Created trade plan with {count} entries."

    def handle_add_trade(self, msg):
        parts = msg.split()
        if len(parts) < 4: return "Usage: add TICKER SOURCE PHRASE"
        ticker, source = parts[1].upper(), parts[2]
        phrase = " ".join(parts[3:])
        return self.handle_create_plan(f"{ticker} {source} {phrase}")

    def handle_focus_trades(self, msg):
        out = [f"{t.ticker} ({t.source}) → {t.score.label} ({t.score.score:.2f})" for t in self.context.intents if t.score.score >= 0.90]
        return "\n".join(out) or "No focus trades found."

    def handle_review(self, msg):
        by_score = {}
        for t in self.context.intents:
            key = f"{t.source}-{t.score.label}"
            if key not in by_score: by_score[key] = {"count": 0, "wins": 0, "pnl": 0}
            by_score[key]["count"] += 1
            if t.outcome == "win": by_score[key]["wins"] += 1
            if t.pnl: by_score[key]["pnl"] += t.pnl
        return "\n".join(f"{k}: {v['count']} trades, {v['wins']} wins, PnL {v['pnl']:.2f}" for k, v in by_score.items())

    def handle_tag_outcome(self, msg):
        try:
            _, _, ticker, outcome, pnl = msg.split()
            for t in self.context.intents:
                if t.ticker == ticker.upper():
                    t.outcome = outcome
                    t.pnl = float(pnl)
                    j = f"[{datetime.datetime.now().isoformat()}] Tagged {ticker} as {outcome} with PnL {pnl}"
                    self.context.journal.append(j)
                    return f"✓ {j}"
            return "Trade not found."
        except:
            return "Usage: tag outcome TICKER win/loss/breakeven PnL"

    def handle_show_plan(self, msg): return "\n".join(f"{t.ticker} {t.source} {t.score.label} ({t.score.score:.2f})" for t in self.context.intents)

    def handle_filter_plan(self, msg):
        parts = msg.split()
        if len(parts) < 4: return "Usage: filter plan SOURCE MIN_SCORE"
        source, threshold = parts[2], float(parts[3])
        return "\n".join(f"{t.ticker} {t.score.label} ({t.score.score:.2f})" for t in self.context.intents if t.source == source and t.score.score >= threshold)

    def handle_summarize_performance(self, msg):
        tally = {}
        for t in self.context.intents:
            label = t.score.label
            tally[label] = tally.get(label, 0) + 1
        return "\n".join(f"{k}: {v}" for k, v in tally.items())

    def handle_coach(self, msg):
        alerts = []
        if sum(1 for t in self.context.intents if t.outcome == "loss") >= 3:
            alerts.append("⚠️ 3+ losses detected — possible revenge trading.")
        if len(self.context.intents) > 10:
            alerts.append("⚠️ Overtrading — more than 10 trades.")
        if all(t.score.label == "Avoid" for t in self.context.intents):
            alerts.append("⚠️ Pattern of low-quality setups — review quality threshold.")
        return "\n".join(alerts) or "No coaching issues."

if __name__ == "__main__":
    trader = IntentTrader()
    print("Intent Trader v0.6.0 final ready.")
    while True:
        user_input = input("\n> ")
        if user_input.lower() in ["quit", "exit"]:
            break
        print(trader.route(user_input))
