---
Front matter
---

# How to Design an IAA Application for Chat
Here’s how a solo dev can turn your requirements and domain model into an IAA-based chat app using the MVP template, while staying 100% true to the IAA spirit (no framework bloat, no “command bus” indirection).


## Step 1: Requirements → Feature List

How to translate requirements for IAA:
1.	Identify Main User “Actions” (Intents)
- Every actionable item or analysis is an intent.
- Source: Commands catalog, domain model entities, real trading workflow.
2.	Map System Outputs to App “Phases” or Context
- Every plan, analysis, or generated output becomes a phase/context variable.
- Source: PLAN, FOCUS, EXECUTE domains; e.g., “trade plan”, “market analysis”, “active trade idea”, etc.
3.	Distill Each Intent to its Core Handler
- Every command or function (e.g., analyze DP, create plan, review trade) gets a handler.
- The handler manages input parsing, logic, and output formatting for its intent.

⸻

## Step 2: Example Mapping from Your Materials

### A. Intents (User Actions to Implement)

(Directly adapted from your /commands.md and workflow)
- clean_transcript: Clean a DP transcript
- analyze_dp: Analyze a DP morning call
- summarize_mancini: Extract data from Mancini newsletter
- analyze_mancini: Analyze Mancini newsletter
- create_plan: Generate unified trade plan
- review_trade: (Post-trade review/management)
- list_setups: List all focus trade ideas
- extract_levels: Extract levels from a given source
- size_position: Calculate appropriate position size
- add_trade: Log/add a trade to the record

Note: Don’t implement as “slash” commands; in IAA, these are natural-language intents like “analyze today’s DP call,” “give me a summary of the Mancini newsletter,” or “what’s the unified plan?”

⸻

### B. State/Context Variables
phase: PLAN, FOCUS, EXECUTE, MANAGE, REVIEW, COACH
last_cleaned_transcript
dp_analysis
mancini_summary
mancini_analysis
trade_plan
active_trade_ideas
level_framework
trade_log

⸻

### C. Handler Example Pseudocode
Handler: handle_analyze_dp

Input: Message like “Analyze today’s DP call: [transcript]”

Output: Dict with dp_analysis in context, response summarizing main points

Handler: handle_create_plan

Input: “Create today’s trade plan”

Logic: Uses dp_analysis, mancini_analysis from context if available

Output: Updates trade_plan in context, returns markdown summary

⸻

## Step 3: Extend the IAA Template

### 1. Add New Intents
- Update define_intent_patterns with keywords per action.
Example:

```python
def define_intent_patterns(self):
    return {
        'CLEAN_TRANSCRIPT': ['clean', 'transcript', 'fix'],
        'ANALYZE_DP': ['analyze', 'dp', 'prince'],
        'SUMMARIZE_MANCINI': ['summarize', 'mancini', 'newsletter'],
        'ANALYZE_MANCINI': ['analyze', 'mancini'],
        'CREATE_PLAN': ['create', 'plan', 'trade plan'],
        # etc...
    }
```


### 2. Add Handler Methods
- Each handler manages one analysis/intent, reads/writes context, and formats output.
- Keep all business logic and state in the handler—no external “framework” needed.

### 3. Decide on Context Format
- For MVP, context stays a string—but you’ll add new fields for each new phase/output as needed.

### 4. Keep Input Natural
- Your MVP should not require users to know slash commands—support “create plan for today”, “analyze this DP call”, etc.

⸻

## Step 4: Implementation Checklist
1. Collect ALL required actions (intents) from docs, domain model, workflow
2. Design a context dict that stores all outputs/objects you want to persist (see above)
3. Define keywords for each intent
4. Write one handler per intent (1:1 with command or output)
5. Connect the dots: in process_message, intent is matched, handler is called, context updated
6. Test: Run through typical workflow (“clean transcript → analyze DP → summarize Mancini → analyze Mancini → create plan”)
7. Polish: Add helpful fallback handler, debug flag, and sample usage in __main__

⸻

## Final Notes
- No framework or command bus. Your IAA app is readable, hackable, and never needs more than one file.
- Your “domain model” and “commands” are not code—they’re just what you turn into intents and context variables!
- Scale up? Add more intents and context fields—never break the template.

⸻

Want an Example?

If you like, I can show a concrete code stub for:
	•	One or two new intents (e.g., “create plan”, “analyze dp”)
	•	The right context dict
	•	Handler skeletons (ready for you to plug in your logic)

Just say the word!