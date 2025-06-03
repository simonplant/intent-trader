# Intent Trader - TODO & Backlog

**Version:** 1.0.0  
**Date:** 2024-05-29 
**Status:** MVP testing & iterating

> **Note:** Completed items are now tracked in `CHANGELOG.md`. Review this backlog weekly to keep priorities fresh and actionable.

## Mission
Ship a production-ready, solo trader-focused, no-bloat trading assistant implementing all PFEMRC workflow, dual scoring, journaling, context save/load, and robust assistant features.

---

## Actionable Backlog

| Task                                      | Status    | Priority | P&L Impact | Notes                        |
|--------------------------------------------|-----------|----------|------------|------------------------------|
| Add chart analysis IAA                     | Doing     | 1        | Direct     | Highest impact: faster, clearer trade reviews and setups. Immediate P&L benefit. |
| Integrate Manchini's trading style/setups  | Planned   | 2        | Direct     | Will improve futures/ES trade selection. High P&L potential if you trade ES. |
| Integrate DP/Inner Circle's trading style  | Planned   | 3        | Direct     | Will improve stock/ETF trade selection. High P&L potential if you trade equities. |
| SPX Options trading handler                | Backlog   | 6        | Direct     | Only if you plan to trade options this week. Otherwise, lower priority. |
| Automate morning plan/focus workflows      | Planned   | 4        | Indirect   | Saves time, reduces errors, but less direct P&L impact than above. |
| Auto-fetch premarket prices from APIs      | Planned   | 5        | Direct     | Reduces manual work, helps with premarket prep. Some P&L impact. |
| Structured end of day analysis/logging     | Planned   | 7        | Indirect   | Improves review, but P&L impact is longer-term. |
| Trade Plan performance by conviction report| Backlog   | 8        | Indirect   | Useful for review, but not urgent for next week. |
| Auto-journal trades with screenshots       | Backlog   | 9        | Indirect   | Good for learning, but not urgent for next week. |
| End of daily report generation             | Backlog   | 10       | Indirect   | Useful for review, but not urgent for next week. |
| End of day Coaching IAA/handler code       | Backlog   | 11       | Indirect   | Behavioral improvement is valuable, but not immediate P&L. |
| Persistent KB incorporated                 | Backlog   | 12       | Indirect   | Longer-term benefit, not urgent. |
| Usage analytics & improvement flywheel     | Parked    | -        | Unclear    | Not urgent. |

---

**Status: SHIPPED! 🎉**

The Intent Trader MVP is 100% complete and production-ready. All 15 requirements met, all tests passing, fully documented. Ready for live trading starting tomorrow.

---

## Agile Task Status Reference

- **Backlog**: Not yet prioritized or ready to work
- **Planned**: Prioritized and ready to work next
- **Doing**: Currently being worked on
- **Review**: Work is done, needs review/testing
- **Done**: Complete and in use
- **Blocked**: Cannot proceed due to a dependency or issue
- **Parked**: Deferred, not being worked on now


# BACKLOG OF NEW IDEAS - TO BE INTEGRATED
- Keep as single file or refactor to multiple files. research best practices via AI.
- Trading charter
- Capital amount and sizing
- parameterize the system (e.g. chart colors, capital)
- Process for how to get regular price updates into the app (e.g. hourly reminder)
- Momentum trading rules bullish > 8 & 21 SMA on 2m
- Clean DPs transcript - tickers, names, company names to tickers, frequent errors.
-  today’s premarket news flow and “most active”/“biggest movers” lists found via financial news sites (Investing.com, Barrons, Yahoo, etc.) using a web lookup
- 