"""
Intent Trader IAA - Solo Dev Canonical Scaffold

A single-file, zero-dependency, readable and hackable trading assistant for solo builders.
All phases, all handlers present as stubs. Add logic as you go.

Author: [Your Name]
Date: [Today]
"""

import re

class IntentTraderAssistant:
    """Your entire trading system in one file—no frameworks, no magic."""

    def __init__(self):
        self.phases = ['PLAN', 'FOCUS', 'EXECUTE', 'MANAGE', 'REVIEW']
        # Initial context: add new fields as you expand
        self.default_context = {
            'phase': 'PLAN',
            'dp_analysis': '',
            'mancini_analysis': '',
            'trade_plan': '',
            'positions': [],
            'trade_log': [],
            'pnl': 0.0
        }
        # Intent keyword mapping: tune and expand as needed
        self.intent_patterns = {
            'ANALYZE_DP':      ['dp', 'prince', 'morning call'],
            'ANALYZE_MANCINI': ['mancini', 'newsletter', 'es futures'],
            'CREATE_PLAN':     ['create plan', 'trade plan', 'daily plan'],
            'ADD_TRADE':       ['add trade', 'buy', 'sell', 'long', 'short'],
            'LIST_POSITIONS':  ['positions', 'my trades', 'open positions'],
            'CLOSE_POSITION':  ['close', 'exit', 'sell', 'cover'],
            'REVIEW':          ['review', 'session', 'recap'],
        }
        # Map intents to handler functions
        self.handlers = {
            'ANALYZE_DP': self.handle_analyze_dp,
            'ANALYZE_MANCINI': self.handle_analyze_mancini,
            'CREATE_PLAN': self.handle_create_plan,
            'ADD_TRADE': self.handle_add_trade,
            'LIST_POSITIONS': self.handle_list_positions,
            'CLOSE_POSITION': self.handle_close_position,
            'REVIEW': self.handle_review,
        }

    # === Intent Routing ===

    def process_message(self, message, context_str="", debug=False):
        ctx = self.parse_context(context_str)
        intent = self.detect_intent(message, debug)
        handler = self.handlers.get(intent, self.handle_unknown)
        response, new_ctx = handler(message, ctx)
        return {
            'response': response,
            'context': self.compress_context(new_ctx),
            'intent': intent
        }

    def detect_intent(self, message, debug=False):
        msg = message.lower()
        for intent, keywords in self.intent_patterns.items():
            for kw in keywords:
                if re.search(rf"\b{re.escape(kw)}\b", msg):
                    if debug:
                        print(f"Matched: '{kw}' → {intent}")
                    return intent
        if debug:
            print("No intent matched, falling back to UNKNOWN.")
        return 'UNKNOWN'

    # === Context Compression/Parsing ===

    def parse_context(self, context_str):
        if not context_str:
            return self.default_context.copy()
        ctx = {}
        for part in context_str.split('|'):
            if ':' in part:
                k, v = part.split(':', 1)
                if k in ['positions', 'trade_log']:
                    ctx[k] = v.split(';;') if v else []
                elif k == 'pnl':
                    ctx[k] = float(v)
                else:
                    ctx[k] = v
        for k in self.default_context:
            if k not in ctx:
                ctx[k] = self.default_context[k]
        return ctx

    def compress_context(self, ctx):
        parts = []
        for k, v in ctx.items():
            if isinstance(v, list):
                parts.append(f"{k}:{';;'.join(str(x) for x in v)}")
            else:
                parts.append(f"{k}:{v}")
        return '|'.join(parts)

    # === Handler Stubs (fill in with logic as needed) ===

    def handle_analyze_dp(self, message, ctx):
        """Stub: Analyze DP's morning call"""
        ctx['dp_analysis'] = '[STUB]'
        ctx['phase'] = 'PLAN'
        return ("DP call analyzed (stub).", ctx)

    def handle_analyze_mancini(self, message, ctx):
        """Stub: Analyze Mancini newsletter"""
        ctx['mancini_analysis'] = '[STUB]'
        ctx['phase'] = 'PLAN'
        return ("Mancini newsletter analyzed (stub).", ctx)

    def handle_create_plan(self, message, ctx):
        """Stub: Generate a unified trade plan"""
        ctx['trade_plan'] = '[STUB]'
        ctx['phase'] = 'FOCUS'
        return ("Trade plan created (stub).", ctx)

    def handle_add_trade(self, message, ctx):
        """Stub: Add a trade position"""
        ctx['positions'].append('[STUB]')
        ctx['trade_log'].append('[STUB]')
        ctx['phase'] = 'EXECUTE'
        return ("Trade added (stub).", ctx)

    def handle_list_positions(self, message, ctx):
        """Stub: List all open positions"""
        pos = ctx.get('positions', [])
        if not pos or pos == ['']:
            return ("No open positions (stub).", ctx)
        return (f"Open positions: {pos}", ctx)

    def handle_close_position(self, message, ctx):
        """Stub: Close a position"""
        ctx['phase'] = 'MANAGE'
        return ("Position closed (stub).", ctx)

    def handle_review(self, message, ctx):
        """Stub: Review session/trades"""
        ctx['phase'] = 'REVIEW'
        return ("Session review (stub).", ctx)

    def handle_unknown(self, message, ctx):
        """Fallback for unknown intents"""
        return ("Sorry, I didn't understand. Try 'analyze dp', 'add trade', 'review', etc.", ctx)

# === Example Usage ===

if __name__ == "__main__":
    bot = IntentTraderAssistant()
    context = ""
    print("\n1. Analyze DP:")
    out = bot.process_message("analyze dp morning call", context)
    print(out['response'])
    context = out['context']

    print("\n2. Analyze Mancini:")
    out = bot.process_message("analyze mancini newsletter", context)
    print(out['response'])
    context = out['context']

    print("\n3. Create Trade Plan:")
    out = bot.process_message("create trade plan", context)
    print(out['response'])
    context = out['context']

    print("\n4. Add Trade:")
    out = bot.process_message("buy 100 NVDA at 900", context)
    print(out['response'])
    context = out['context']

    print("\n5. List Positions:")
    out = bot.process_message("show my positions", context)
    print(out['response'])

    print("\n6. Close Position:")
    out = bot.process_message("close NVDA at 925", context)
    print(out['response'])
    context = out['context']

    print("\n7. Review Session:")
    out = bot.process_message("review session", context)
    print(out['response'])

    print("\nFinal Context:", context)
    