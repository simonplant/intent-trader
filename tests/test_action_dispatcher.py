import pytest
from core.action_dispatcher import ActionDispatcher

def test_action_dispatcher():
    dispatcher = ActionDispatcher()
    result = dispatcher.dispatch("clean_transcript", content="dummy content")
    assert result["status"] == "success" 