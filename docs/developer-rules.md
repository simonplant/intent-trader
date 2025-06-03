---
Name: Developer Rules for Intent Trader
---

# Developer rules to be followed by AI assistants when developing new code for my systems.
- These are solo apps. MVP.
- No enterprise fuckery
- No bloat.
- Best practices only in the pragmatic and simplificatopm context.
- Dont over engineer
- Use architecture best practices in `docs/iaa-architecture-best-practices.md`s
- The app code is the master. You dont change the app to work with the tests, fix the tests unless there is truly an issue with the app code. The tester serves the app as the master.

Best design for a solo developer means:
- Simplicity
- Clarity
- Works reliably
- Less code
- Less abstraction
- Fewer dependencies
- Fewer files
- Simple to debug
- Can still understand it in 6 months
- Ships TODAY
- No exterprise bloat, complexity, tons of files, "best practices".