# INTENT TRADER — MVP LAUNCH OVERHAUL

**Mission:** Ship a production-ready, solo trader-focused, no-bloat trading assistant implementing all PFEMRC workflow, dual scoring, journaling, context save/load, and robust assistant features.

---

## MUST-HAVE MVP LAUNCH (Finish ALL Before Release)

- [ ] PFEMRC phases: Plan, Focus, Execute, Manage, Review, Coach — all as explicit handlers
- [ ] Data models: Modern `dataclasses` for TradePlan, TradeIntent, ConvictionScore; all state must be type-safe and auditable
- [ ] Dual scoring: Attach analyst & system conviction (0.0–1.0) to every trade/intent
- [ ] Intent/phase registry: Handlers are cleanly mapped and extensible
- [ ] NLP intent detection: Fast keyword/phrase mapping to workflow phases and commands
- [ ] All assistant utilities: Save, Load, Journal, Help, Context, Reset — always available
- [ ] In-memory journaling: Timestamped entries for every trade and note, viewable on demand
- [ ] Context flatten/restore: Can save and load the full trading session (JSON) in a single command
- [ ] Position & PnL tracking: Positions and basic running PnL stored in plan state
- [ ] Behavioral pattern checks: Alerts for revenge/tilt/stop-out, toggleable or extensible
- [ ] Clean markdown output: All responses and summaries formatted for easy reading and pasting
- [ ] Full error handling: Type/value checks, user-facing errors only, never corrupt or crash state
- [ ] Quick context/session reset: Fresh start at any time, zero confusion or legacy data
- [ ] Minimal unit/integration tests: Each handler and full workflow path tested once
- [ ] Usage docs: One example session in README, command list, save/load/journal demo

---

## “DONE IS BETTER THAN PERFECT” GUIDELINES

- No broker APIs, no DB, no cloud, no analytics/dashboard — only hooks for later
- No legacy or CLI code, no in-line user manual bloat — README and help command only
- No multi-user logic, no permissions, no extra abstraction layers
- Everything extensible, nothing over-architected

---

## DONE (Track completion here as you go)

- [ ] (Copy MVP tasks here as you finish each for true progress tracking!)

---

**When every box is checked, you are production-ready and can trade, review, and evolve your own system with zero tech debt and full transparency.**