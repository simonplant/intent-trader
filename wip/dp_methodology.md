---
title: DP Trading Methodology
description: Comprehensive analysis of DP's trading approach
author: Simon Plant
date: 2024-05-28
---

# Comprehensive Analysis of DP's Trading Methodology

## Objective

DP is considering retiring and potentially closing the Inner Circle trading room. To prepare for independent trading, I seek a comprehensive understanding of DP's trading methodology. My goal is to deeply internalize the strategies, frameworks, and execution techniques that DP consistently employs to identify and execute profitable trades, manage risk, and adapt to varying market conditions.

## Trade Identification

How DP Finds Trades:
- Focus on Market Inflection Points:
DP consistently scans for spots where the prevailing market narrative is likely to break or reverse. This is rarely about chasing momentum, but about catching where consensus is wrong—particularly around major news, earnings, or when sentiment is stretched.
"Watch for market to fail at the 21d MA; everyone's leaning one way, risk is a squeeze."
- Leverages Institutional Positioning:
He reads the market through the lens of hedge fund flows, options positioning, and institutional sentiment. If funds are crowded in one direction and a catalyst is coming, DP looks for the opposite move.
"Hedge funds are max long; that's often when we see reversals if the data disappoints."
- Differentiates 'Priced In' from 'Unpriced':
A core skill is parsing what information the market already knows and has discounted versus what is new, unexpected, or misinterpreted.
"This move is just shorts covering, nothing new is being priced in yet."
- Sentiment and News Flow:
Constantly evaluates news flow for shifts in tone or new risk events. Recognizes when panic or euphoria is overdone and positions accordingly.
"The downgrade is noisy, but watch if the tape doesn't break—could be a fade setup."
- Key Levels and Triggers:
Uses technical levels (MAs, prior highs/lows) as triggers for trades, not as the reason for the trade. They're decision points, not predictive tools.

## Trade Execution and Management

How DP Enters, Adjusts, and Exits:
- Trading Around a Core:
Rarely all-in, all-out. DP establishes a core position at his preferred price, then trades around it—adding on dips, trimming into strength, re-adding if the thesis strengthens.
"Took a starter, will add if it dips to 50d; trimming if it gets extended."
- Tiered Approach:
Builds into size in steps rather than all at once—uses market weakness or failed moves to increase size only when the trade is working or the risk/reward improves.
"Started 1/3 size. If it confirms above 8d, will go full."
- Defined Exits and Active Management:
Has clear technical or news-based stops, but also adapts in real-time. Exits if thesis fails, trims if the market gets frothy, lets a portion run for bigger moves.
- Scalps vs. Swings:
Differentiates between short-term, intraday "cashflow" trades and multi-day "swing" ideas. Scalps are quick, against emotional moves or at key levels; swings are based on mispricing or multi-day themes.
- Partial Profits:
Takes partials on the way up, books profits incrementally, never waits for perfection.

## Risk Management Practices

How DP Manages Risk:
- Sizing by Conviction and Volatility:
Highest conviction = biggest size (often double), but still tiers in. Low conviction or higher volatility = smaller size, wider stops.
- Defined Risk per Trade:
Knows exactly where he's wrong before entry. Never adds to losing trades unless planned as part of the thesis.
- Stop-Loss and Mental Stops:
Uses a mix of hard stops (for binary events/news) and mental stops (for market structure breaks). Flexible but never undisciplined.
- Adaptable to Market Regimes:
Risk is dialed up or down depending on volatility and clarity of opportunity.
"Market is choppy, I'm keeping risk light and quick to trim."

## Adaptation to Market Conditions

How DP Aligns Trading Style:
- Multiple Styles:
Trades scalps, day trades, swings, and long-term holds—but is explicit about which regime he's operating in and adjusts tactics accordingly.
"This is just a cashflow scalp, don't marry it."
"Swing setup if we close above 21d."
- Market Environment Dictates Tactics:
When volatility is high, DP is faster to trim and cut; when trend is strong and clear, he holds longer and builds size.
- Recognizes 'No-Trade' Zones:
Disciplined about not forcing trades in chop or unclear conditions.
"Sometimes the best trade is not trading."
- Willingness to Flip Bias:
DP isn't dogmatic—he'll reverse a trade or bias quickly if the market structure changes.

## Behavioral Insights and Discipline

Psychological Edges:
- Detachment from Ego:
Willing to be wrong quickly—exits or flips rather than hoping or averaging down emotionally.
- Patient for Setups:
Waits for his price and setup; doesn't chase.
"Missed my fill, moving on."
- Exploits Emotional Extremes:
Looks for opportunities when others panic or get greedy—often fading capitulation or euphoria.
- Objective Self-Review:
Reviews trades, learns from misses and mistakes, and adjusts rules. Keeps a "clean slate" mentality each day.

Illustrative Examples (Quotes & Paraphrases)
- "No reason to chase; if it pulls back to the 8d and holds, I'll get involved."
- "Everyone is leaning bearish, but if the numbers aren't as bad as feared, could squeeze."
- "Booked half; will let the rest ride with a stop at break-even."
- "This is noise, not new information. Standing aside."
- "Cut quick if thesis fails, add only when the market confirms."

DP's Core Methodology (Summary for Independent Use)
1.	Find Trades by Looking Where the Market is Wrong:
Focus on inflection points, mispricings, and changes in consensus, not just chart patterns or momentum.
2.	Execute with Discipline and Flexibility:
Build around a core, scale in and out, take partials, and actively manage both entries and exits. Let trades "work for you," but don't marry them.
3.	Manage Risk Relentlessly:
Size by conviction and volatility, know your stop before entry, and never add to losers unless planned.
4.	Match Trading Style to Market Conditions:
Use scalps for noise and extremes, swings for mispricings, and stand aside in chop. Adapt your bias as the tape changes.
5.	Maintain Emotional Edge:
Stay detached, patient, and self-reflective. Exploit emotional overreactions, review your trades, and never let ego dictate action.


# DPs Methodology
Here’s a DP Methodology Guide for Trading—modeled after the structure of your IAA application design file, using clear phases and step-by-step breakdowns for “productionizing” DP’s process.
It is written for independent trading but can be easily mapped to software, workflows, or system building.

⸻

## How to Apply DP’s Methodology from Trade Idea to Execution

Based on years of DP transcripts, here’s a production-ready guide for turning raw market information into consistent, high-quality trading results.

⸻

### Phase 1: Narrative, Sentiment & Trade Identification

1.1 Identify Inflection Points (“Intent”)

Every actionable trade idea starts with recognizing:
	•	What is priced in: What does the market “know” and expect?
	•	What is not priced in: Where is consensus vulnerable? What’s the surprise risk or reward?
	•	Who is positioned how?: Are hedge funds and institutions crowded? Is sentiment extreme?
	•	Key Levels and Triggers: What are the market’s most watched technical and psychological levels?

DP Practice Example:
“Everyone’s bearish on CPI, but the chart won’t break—watch for a squeeze if data isn’t awful.”


1.2 Map Trade Ideas to Playbooks

Every observation leads to a specific playbook:
	•	Catalyst Trade (earnings, news, macro event)
	•	Sentiment Reversal (crowded positioning, too bearish/bullish)
	•	Technical Break or Hold (key moving average, range high/low)
	•	Fade the Extreme (panic selling, euphoria buying)

Pseudocode
```python
if catalyst and sentiment extreme:
    setup = "reversal"
elif new news and price at key level:
    setup = "breakout or fail"
```

### Phase 2: Build Position & Execute

2.1 Start with a Core
	•	Take a starter position at a good risk/reward price.
“Starter at the 21d, will add if thesis confirms.”

2.2 Trade Around the Core
	•	Scale in if price improves or thesis strengthens.
	•	Trim if market gives you an outsized move quickly.
	•	Re-add on pullbacks if core thesis remains valid.
	•	Use tiers for high conviction—never all-in, all-out.

Example
```python
size = "starter"
if setup works and market confirms:
    size = "full"
if quick move or level fails:
    trim()
```


### Phase 3: Risk Management

3.1 Define the Downside Before Entry
	•	Set clear, actionable stop levels (technical, news, thesis-based).
	•	Position size by conviction and volatility—big only when the setup is best.
	•	Never add to losers unless it’s part of your pre-defined plan.

Pseudocode
```python
risk = max_loss_per_trade
if loss hits risk:
    exit()
if multiple stops in a day:
    stand down
```

3.2 Adapt as Tape Changes
	•	Be ready to cut quickly if the market “proves you wrong.”
	•	Stay light in choppy environments, heavier in trending tape.


### Phase 4: Match Style to Market Conditions

4.1 Choose Your Trading Style per Environment
	•	Scalp: Quick moves at key levels (news, sentiment extremes)
	•	Day Trade: Full session swing, often news- or level-driven
	•	Swing: Multi-day to multi-week, based on mispricing or major positioning
	•	Long-term: Rare, when fundamental/structural mispricing is clear

“This is a scalp only; don’t marry it.”
“Swing only if closes above 21d.”

4.2 Know When NOT to Trade
	•	If tape is choppy, signals are mixed, or risk is unclear—sit out.
	•	If thesis changes, exit and move on.
“Sometimes the best trade is no trade.”


### Phase 5: Behavioral Discipline & Review

5.1 Detachment & Emotional Edge
	•	No ego trades: Be willing to flip or stand down.
	•	Wait for your price: Don’t chase.
	•	Exploit emotion: Fade panic/euphoria, but only with setup.

5.2 Post-Trade Review
	•	Log every trade—win or lose.
	•	Review setups, mistakes, and market context.
	•	Start each day with a clean slate; never carry baggage forward.

Example review fields
```python
trade_log.append({
    'ticker': ticker,
    'setup': setup,
    'entry': entry,
    'exit': exit,
    'pnl': pnl,
    'what worked': notes,
    'what failed': lessons
})
```


## Final Notes
- Your edge is not in the news, but in how you read the tape and position around it.
- Be flexible. Market changes fast; so should you.
- Always know your risk. Protect capital first—profit comes from consistency and longevity.

⸻

## The DP Method summary
1.	Find trades where the crowd is wrong and risk/reward is best.
2.	Build positions in tiers, trade around a core, and let winners work.
3.	Manage risk and adapt as the market changes.
4.	Match your trading style to the conditions—be a sniper, not a machine gun.
5.	Maintain discipline, review relentlessly, and never let ego override process.
