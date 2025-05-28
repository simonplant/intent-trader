import pytest

from core.intent_parser import Intent, IntentParser


def test_intent_parser():
    parser = IntentParser()
    intent = parser.parse("plan from dp transcript")
    assert intent.action == "plan"
    assert intent.parameters["source"] == "dp_transcript"
