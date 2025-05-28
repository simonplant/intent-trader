# Intent-Trader

An Intent-Agent-Action (IAA) trading assistant that ingests, analyzes, and synthesizes trading plans from DP transcripts and Mancini PDFs.

## Overview

Intent-Trader is a modular, schema-driven IAA app designed to run in Claude or ChatGPT chat apps (desktop). It processes trading transcripts and PDFs, extracts actionable insights, and generates unified trade plans.

## Features

- **Intent Parsing**: Converts user input or workflow triggers into structured intents.
- **Modular Agents**: Planner, Analyst, ConvictionClassifier, and more.
- **Schema-Driven Data**: Validates all inputs and outputs.
- **Prompt Templates**: Version-controlled, parameterized prompts.
- **File-Based Storage**: Simple, low-complexity data storage.

## Setup

1. Clone the repo:
   ```bash
   git clone https://github.com/yourusername/intent-trader.git
   cd intent-trader
   ```

2. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

3. Run tests:
   ```bash
   python -m pytest tests/
   ```

## Usage

- **Plan Workflow**: Ingest DP transcript → clean → analyze → output plan.
- **More workflows coming soon!**

## License

MIT