from src.core.action_dispatcher import ActionDispatcher
from src.core.intent_parser import Intent


def test_action_dispatcher():
    dispatcher = ActionDispatcher()
    
    # Register a handler for clean_transcript action
    def clean_transcript_handler(intent):
        return {"status": "success", "action": "clean_transcript", "content": "cleaned"}
    
    dispatcher.register_handler("clean_transcript", clean_transcript_handler)
    
    # Create an intent
    intent = Intent(action="clean_transcript", parameters={"content": "dummy content"})
    
    result = dispatcher.dispatch(intent)
    assert result["status"] == "success"
