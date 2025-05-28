"""
Generic IAA (Intent-Aware Assistant) Template
A minimal, chat-native assistant framework that can be adapted for any domain.
No bloat. No dependencies. Just pure logic.
"""

import json
import re
from typing import Dict, List, Optional, Tuple, Any
from abc import ABC, abstractmethod

# Base IAA Framework
class IntentAwareAssistant(ABC):
    """Base class for building Intent-Aware Assistants."""
    
    def __init__(self):
        self.phases = self.define_phases()
        self.intent_patterns = self.define_intent_patterns()
        self.handlers = self.define_handlers()
    
    @abstractmethod
    def define_phases(self) -> List[str]:
        """Define the phases/states for your assistant."""
        pass
    
    @abstractmethod
    def define_intent_patterns(self) -> Dict[str, List[str]]:
        """Define keyword patterns for intent detection."""
        pass
    
    @abstractmethod
    def define_handlers(self) -> Dict[str, callable]:
        """Map intents to handler functions."""
        pass
    
    def process_message(self, message: str, context: str = "") -> Dict[str, str]:
        """Main entry point - process any message."""
        ctx = self.parse_context(context)
        intent = self.detect_intent(message.lower())
        
        # Route to appropriate handler
        handler = self.handlers.get(intent, self.handle_unknown)
        response, new_ctx = handler(message, ctx)
        
        return {
            'response': response,
            'context': self.compress_context(new_ctx),
            'intent': intent
        }
    
    def parse_context(self, context_str: str) -> Dict:
        """Parse compressed context string."""
        if not context_str:
            return self.get_default_context()
        
        ctx = {}
        parts = context_str.split('|')
        
        for part in parts:
            if ':' in part:
                key, value = part.split(':', 1)
                ctx[key] = self.parse_context_value(key, value)
        
        return ctx
    
    def compress_context(self, ctx: Dict) -> str:
        """Compress context to minimal string."""
        parts = []
        for key, value in ctx.items():
            compressed_value = self.compress_context_value(key, value)
            parts.append(f"{key}:{compressed_value}")
        return '|'.join(parts)
    
    def detect_intent(self, message: str) -> str:
        """Detect intent from message using patterns."""
        for intent, keywords in self.intent_patterns.items():
            if any(kw in message for kw in keywords):
                return intent
        return self.get_default_intent()
    
    @abstractmethod
    def get_default_context(self) -> Dict:
        """Return default context structure."""
        pass
    
    @abstractmethod
    def get_default_intent(self) -> str:
        """Return default intent when none detected."""
        pass
    
    @abstractmethod
    def parse_context_value(self, key: str, value: str) -> Any:
        """Parse a context value from string."""
        pass
    
    @abstractmethod
    def compress_context_value(self, key: str, value: Any) -> str:
        """Compress a context value to string."""
        pass
    
    @abstractmethod
    def handle_unknown(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        """Handle unknown intent."""
        pass


# Example Implementation: Task Management IAA
class TaskAssistant(IntentAwareAssistant):
    """Example implementation for task management."""
    
    def define_phases(self) -> List[str]:
        return ['PLANNING', 'WORKING', 'REVIEWING']
    
    def define_intent_patterns(self) -> Dict[str, List[str]]:
        return {
            'CREATE': ['new', 'create', 'add', 'task', 'todo'],
            'UPDATE': ['update', 'change', 'modify', 'edit'],
            'COMPLETE': ['done', 'complete', 'finished', 'check'],
            'LIST': ['show', 'list', 'what', 'tasks', 'todos'],
            'PRIORITIZE': ['priority', 'important', 'urgent', 'focus'],
            'REVIEW': ['review', 'summary', 'progress', 'status']
        }
    
    def define_handlers(self) -> Dict[str, callable]:
        return {
            'CREATE': self.handle_create,
            'UPDATE': self.handle_update,
            'COMPLETE': self.handle_complete,
            'LIST': self.handle_list,
            'PRIORITIZE': self.handle_prioritize,
            'REVIEW': self.handle_review
        }
    
    def get_default_context(self) -> Dict:
        return {
            'phase': 'PLANNING',
            'tasks': [],
            'completed': 0
        }
    
    def get_default_intent(self) -> str:
        return 'LIST'
    
    def parse_context_value(self, key: str, value: str) -> Any:
        if key == 'phase':
            return value
        elif key == 'tasks':
            # Format: title:priority:status,title2:priority2:status2
            tasks = []
            if value:
                for task_str in value.split(','):
                    parts = task_str.split(':')
                    if len(parts) >= 3:
                        tasks.append({
                            'title': parts[0],
                            'priority': parts[1],
                            'status': parts[2]
                        })
            return tasks
        elif key == 'completed':
            return int(value) if value else 0
        return value
    
    def compress_context_value(self, key: str, value: Any) -> str:
        if key == 'tasks':
            task_strs = []
            for t in value:
                task_strs.append(f"{t['title']}:{t['priority']}:{t['status']}")
            return ','.join(task_strs)
        return str(value)
    
    def handle_create(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        # Extract task from message
        task_match = re.search(r'(task|todo)[:s]*(.+)', message, re.IGNORECASE)
        if task_match:
            task_title = task_match.group(2).strip()
            ctx['tasks'].append({
                'title': task_title,
                'priority': 'normal',
                'status': 'pending'
            })
            ctx['phase'] = 'WORKING'
            return f"✅ Created: {task_title}\n→ Total tasks: {len(ctx['tasks'])}", ctx
        return "❌ Please specify a task to create", ctx
    
    def handle_list(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        if not ctx['tasks']:
            return "📋 No tasks yet. Create one with 'new task: [description]'", ctx
        
        response = "📋 Tasks:\n"
        for i, task in enumerate(ctx['tasks'], 1):
            status_icon = '✓' if task['status'] == 'done' else '○'
            priority_icon = '🔴' if task['priority'] == 'high' else ''
            response += f"{status_icon} {task['title']} {priority_icon}\n"
        
        return response, ctx
    
    def handle_complete(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        # Simple implementation - complete first pending task
        for task in ctx['tasks']:
            if task['status'] == 'pending':
                task['status'] = 'done'
                ctx['completed'] += 1
                return f"✅ Completed: {task['title']}", ctx
        return "No pending tasks to complete", ctx
    
    def handle_update(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        return "🔧 Update functionality - specify task and changes", ctx
    
    def handle_prioritize(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        return "🎯 Prioritization - mark tasks as high/normal/low priority", ctx
    
    def handle_review(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        ctx['phase'] = 'REVIEWING'
        pending = len([t for t in ctx['tasks'] if t['status'] == 'pending'])
        response = f"📊 Review:\n"
        response += f"Total: {len(ctx['tasks'])} | Done: {ctx['completed']} | Pending: {pending}\n"
        response += "→ What went well? What to improve?"
        return response, ctx
    
    def handle_unknown(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        return "❓ Try: create task, list tasks, complete, review", ctx


# Generic Template for Custom Implementation
class CustomAssistant(IntentAwareAssistant):
    """Template for creating your own domain-specific assistant."""
    
    def define_phases(self) -> List[str]:
        # Define your workflow phases
        return ['PHASE1', 'PHASE2', 'PHASE3']
    
    def define_intent_patterns(self) -> Dict[str, List[str]]:
        # Define keywords for each intent
        return {
            'INTENT1': ['keyword1', 'keyword2'],
            'INTENT2': ['keyword3', 'keyword4'],
            # Add more intents...
        }
    
    def define_handlers(self) -> Dict[str, callable]:
        # Map intents to handler methods
        return {
            'INTENT1': self.handle_intent1,
            'INTENT2': self.handle_intent2,
            # Add more handlers...
        }
    
    def get_default_context(self) -> Dict:
        # Define your context structure
        return {
            'phase': 'PHASE1',
            'data': [],
            'state': {}
        }
    
    def get_default_intent(self) -> str:
        return 'INTENT1'
    
    def parse_context_value(self, key: str, value: str) -> Any:
        # Custom parsing logic for your context values
        if key == 'data':
            return json.loads(value) if value else []
        return value
    
    def compress_context_value(self, key: str, value: Any) -> str:
        # Custom compression logic
        if key == 'data':
            return json.dumps(value)
        return str(value)
    
    def handle_intent1(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        # Implement your logic
        response = "🎯 Handling Intent 1\n"
        response += "→ Next action?"
        return response, ctx
    
    def handle_intent2(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        # Implement your logic
        response = "📊 Handling Intent 2\n"
        return response, ctx
    
    def handle_unknown(self, message: str, ctx: Dict) -> Tuple[str, Dict]:
        return "❓ Unknown command. Available: intent1, intent2", ctx


# Utility functions that can be reused
def extract_entities(text: str, pattern: str) -> List[str]:
    """Extract entities matching a pattern from text."""
    matches = re.findall(pattern, text)
    return matches

def parse_key_value(text: str, key: str) -> Optional[str]:
    """Extract value for a key from text like 'key: value'."""
    pattern = rf'{key}[:s]*([^,\n]+)'
    match = re.search(pattern, text, re.IGNORECASE)
    return match.group(1).strip() if match else None

def format_list(items: List[str], bullet: str = "•") -> str:
    """Format a list with bullets."""
    return '\n'.join(f"{bullet} {item}" for item in items)


# Example usage
if __name__ == "__main__":
    # Test the task assistant
    assistant = TaskAssistant()
    context = ""
    
    print("=== Task Assistant Demo ===")
    
    # Create task
    resp = assistant.process_message("new task: Review project proposal", context)
    print(f"1. {resp['response']}")
    context = resp['context']
    
    # List tasks
    resp = assistant.process_message("show my tasks", context)
    print(f"2. {resp['response']}")
    context = resp['context']
    
    # Complete task
    resp = assistant.process_message("done with that", context)
    print(f"3. {resp['response']}")
    context = resp['context']
    
    # Review
    resp = assistant.process_message("review my progress", context)
    print(f"4. {resp['response']}")
    
    print(f"\nFinal context: {context}")
    