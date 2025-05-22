# Solo Developer Multi-Agent Workflow: Enterprise SOP

## Document Overview

**Purpose**: Define standard operating procedures for solo developer productivity using coordinated AI assistance across multiple platforms.

**Scope**: Individual software development projects requiring strategic planning, tactical execution, and continuous progress tracking.

**Version**: 2.0 - Solo Developer Edition  
**Last Updated**: January 2025  
**Review Cycle**: Monthly or per project completion

---

## Executive Summary

This SOP documents a proven solo developer workflow that leverages platform-specific AI strengths to maximize individual productivity. The methodology uses ChatGPT for strategic planning and project oversight, Claude for focused implementation work, and lightweight file-based tracking for continuous coordination.

**Key Benefits**:
- **3x Development Velocity**: Strategic planning reduces implementation rework
- **Consistent Quality**: Template-driven prompts ensure reliable output
- **Zero Overhead**: File-based tracking with no external tools required
- **Context Preservation**: Simple status files maintain project continuity

**Total Monthly Cost**: ~$40 (ChatGPT Pro + Claude Pro subscriptions)

---

## System Architecture

### Platform Assignments

#### **ChatGPT Pro + Desktop App (Strategic Layer)**
- **Role**: Project Manager & Development Controller
- **Strengths**: Unlimited context, strategic thinking, task breakdown
- **Usage**: Daily planning, weekly reviews, architectural decisions
- **Access**: Desktop app for always-available quick consultations

#### **Claude Pro + macOS Desktop App (Tactical Layer)**  
- **Role**: Implementation Specialist & Code Author
- **Strengths**: Superior code generation, artifact creation, technical precision
- **Usage**: Feature implementation, debugging, documentation generation
- **Access**: Desktop app for seamless code workflow integration

#### **File-Based State Management (Coordination Layer)**
- **current-status.md**: Agile-style sprint status and daily standup info
- **todo.md**: Comprehensive task backlog with priorities and dependencies
- **worker-prompts.md**: Template library for consistent Claude interactions

### Information Flow

```
ChatGPT (Strategy) → todo.md → Claude (Implementation) → current-status.md → Review Cycle
    ↑                                                                              ↓
    ←←←←←← Weekly Planning ←←← Daily Status ←←← Completed Work ←←←←←←←←←←←←←←←←←←←←←←
```

---

## Core Operating Procedures

### SOP-001: Project Initialization

#### **Prerequisites**
- [ ] ChatGPT Pro subscription active
- [ ] Claude Pro subscription active
- [ ] Desktop apps installed and logged in
- [ ] Repository created with initial structure

#### **Setup Process**

**Step 1: Strategic Context Setup (ChatGPT Desktop)**
```markdown
Project Manager Prompt:
"I'm starting a new solo development project called [PROJECT_NAME].

Project Details:
- Description: [PROJECT_DESCRIPTION]
- Technology: [TECH_STACK]
- Timeline: [ROUGH_TIMELINE]
- Success Criteria: [WHAT_DEFINES_DONE]

I need you to help me:
1. Break this into logical phases/milestones
2. Identify major technical risks early
3. Plan weekly goals and daily focus areas
4. Track progress and adjust timeline as needed

I'll update you daily with what I accomplished and you'll help me plan the next day's work. Ready?"
```

**Step 2: Implementation Context Setup (Claude Desktop)**
```markdown
Code Specialist Prompt:
"I'm building [PROJECT_NAME] - [BRIEF_DESCRIPTION].

Tech Stack: [TECHNOLOGIES]
Repository: [REPO_STRUCTURE]
Coding Standards: [STANDARDS/PATTERNS]

I'll be giving you specific implementation tasks throughout this project. For each task, please provide:
1. Complete, working code
2. Comprehensive tests
3. Clear documentation
4. Brief status summary for my tracking

Always follow these patterns: [SPECIFIC_PATTERNS]
Always include error handling and logging where appropriate.

Confirm you understand the project context and are ready for implementation tasks."
```

**Step 3: File Structure Setup**
```bash
# Create coordination files in project root
touch current-status.md
touch todo.md  
touch worker-prompts.md

# Initialize with templates (see Appendix A)
```

#### **Deliverables**
- [ ] ChatGPT context established with project overview
- [ ] Claude context established with technical details
- [ ] Coordination files created and initialized
- [ ] Initial todo.md populated with high-level tasks

### SOP-002: Daily Development Workflow

#### **Morning Planning Ritual (5 minutes)**

**Step 1: Status Review (ChatGPT Desktop)**
```markdown
Daily Planning Prompt:
"Here's yesterday's status from current-status.md:
[PASTE_CURRENT_STATUS]

And here's my current todo.md:
[PASTE_RELEVANT_TODOS]

Based on this, what should I focus on today? Please:
1. Identify 1-3 priority tasks for today
2. Flag any blockers or dependencies
3. Suggest the logical sequence for today's work
4. Update my understanding of project progress"
```

**Step 2: Task Selection**
- Choose 1-3 specific tasks from ChatGPT's recommendations
- Update todo.md with today's priorities
- Identify which tasks need Claude implementation vs. planning

#### **Implementation Cycle (Variable Duration)**

**Step 3: Task Execution (Claude Desktop)**
```markdown
Implementation Task Prompt Template:
"Task: [TASK_FROM_TODO]

Context: [CURRENT_PROJECT_STATE]
Files to modify/create: [SPECIFIC_FILES]
Success criteria: [HOW_TO_KNOW_ITS_DONE]

Previous work: [WHAT_WAS_BUILT_BEFORE_THIS]
Dependencies: [WHAT_THIS_RELIES_ON]

Please implement this task completely with tests and documentation."
```

**Step 4: Integration & Validation**
- Copy Claude's code to VS Code
- Run tests and validate functionality
- Commit to repository with descriptive message
- Update current-status.md with completion

**Step 5: Status Update**
```markdown
# Add to current-status.md
## [DATE] - [TASK_COMPLETED]
✅ Completed: [DESCRIPTION]
🧪 Tests: [PASSING/FAILING_COUNT]
📝 Notes: [ANY_ISSUES_OR_DECISIONS]
⏭️ Next: [WHAT_THIS_ENABLES]
```

#### **Evening Review (5 minutes)**

**Step 6: Daily Retrospective (ChatGPT Desktop)**
```markdown
End of Day Review Prompt:
"Here's what I accomplished today:
[PASTE_TODAY_STATUS_UPDATES]

Please help me:
1. Assess progress against weekly goals
2. Identify tomorrow's logical next steps  
3. Flag any risks or blockers emerging
4. Update project timeline if needed"
```

**Step 7: Todo Updates**
- Move completed items to "Done" section
- Add new tasks identified during implementation
- Reprioritize based on ChatGPT's assessment
- Prepare tomorrow's focus areas

### SOP-003: Weekly Planning Cycle

#### **Weekly Review Process (30 minutes)**

**Step 1: Progress Assessment (ChatGPT)**
```markdown
Weekly Review Prompt:
"Here's this week's current-status.md entries:
[PASTE_WEEK_STATUS]

And here's the updated todo.md:
[PASTE_CURRENT_TODOS]

Please provide a weekly assessment:
1. Velocity: How much got done vs. planned?
2. Quality: Any patterns in issues or rework?
3. Blockers: What slowed progress this week?
4. Focus: What should drive next week's priorities?
5. Timeline: Are we on track for project goals?"
```

**Step 2: Next Week Planning**
- Review and update project milestones
- Identify 5-10 specific tasks for coming week
- Sequence tasks based on dependencies
- Estimate effort and identify potential blockers

**Step 3: Context Refresh**
- Update ChatGPT with any architectural decisions made
- Refresh Claude context if significant changes occurred
- Clean up todo.md by archiving completed items
- Reset current-status.md for new week

---

## File Management System

### current-status.md Structure

```markdown
# Project Status: [PROJECT_NAME]

## Sprint Goal: [CURRENT_WEEK_OBJECTIVE]
**Start Date**: [WEEK_START]  
**Target Completion**: [WEEK_END]

## Daily Progress

### [DATE] - [DAY_OF_WEEK]
**Focus**: [PLANNED_TASKS]
**Completed**: 
- ✅ [Task 1] - [Brief outcome]
- ✅ [Task 2] - [Brief outcome]

**In Progress**:
- 🔄 [Task 3] - [Current state, blockers]

**Issues/Blockers**:
- ⚠️ [Issue description] - [Impact/resolution plan]

**Tomorrow's Priority**: [1-2 SPECIFIC_TASKS]

---

### [PREVIOUS_DATE] - [DAY]
[Previous day's entry...]

## Weekly Summary
**Velocity**: [X tasks completed / Y planned]
**Quality**: [Issues encountered, lessons learned]  
**Next Week Focus**: [High-level objectives]
```

### todo.md Structure

```markdown
# Todo: [PROJECT_NAME]

## This Week (Priority Order)
- [ ] **HIGH**: [Task] - [Why important] - [Estimate]
- [ ] **MEDIUM**: [Task] - [Dependencies] - [Estimate]  
- [ ] **LOW**: [Task] - [Context] - [Estimate]

## Next Week
- [ ] [Task] - [Rough description]
- [ ] [Task] - [Dependencies to resolve]

## Backlog (Future Weeks)
- [ ] [Feature/Task] - [Context/rationale]
- [ ] [Technical debt item] - [Impact if not addressed]

## Blocked/Waiting
- [ ] [Task] - [What it's waiting for] - [Follow-up date]

## Done (This Week)
- [x] [Task] - [Completion date] - [Outcome]
- [x] [Task] - [Completion date] - [Notes]

## Ideas/Maybe
- [ ] [Nice-to-have feature] - [Why it might be valuable]
- [ ] [Alternative approach] - [Trade-offs to consider]
```

### worker-prompts.md Structure

```markdown
# Worker Prompt Templates

## Implementation Task Template
```
Task: [SPECIFIC_TASK_FROM_TODO]

Context: 
- Project: [PROJECT_NAME]  
- Current state: [WHAT_EXISTS_NOW]
- This task fits into: [LARGER_FEATURE/MILESTONE]

Requirements:
- Input: [WHAT_DATA/PARAMETERS_IT_RECEIVES]
- Output: [WHAT_IT_SHOULD_PRODUCE]  
- Behavior: [HOW_IT_SHOULD_WORK]
- Constraints: [TECHNICAL_LIMITATIONS]

Technical Details:
- Files to modify: [SPECIFIC_FILES]
- Dependencies: [WHAT_IT_RELIES_ON]
- Testing: [HOW_TO_VALIDATE_SUCCESS]

Please provide complete implementation with tests and documentation.
```

## Bug Fix Template  
```
Bug: [DESCRIPTION_OF_PROBLEM]

Current behavior: [WHAT_HAPPENS_NOW]
Expected behavior: [WHAT_SHOULD_HAPPEN]

Context:
- When it occurs: [SPECIFIC_CONDITIONS]
- Error messages: [IF_ANY]
- Files involved: [SUSPECTED_LOCATIONS]

Debugging done so far: [WHAT_IVE_TRIED]

Please provide fix with explanation and test to prevent regression.
```

## Feature Enhancement Template
```
Enhancement: [FEATURE_TO_IMPROVE]

Current implementation: [HOW_IT_WORKS_NOW]
Desired improvement: [WHAT_SHOULD_BE_BETTER]
Success criteria: [HOW_TO_MEASURE_SUCCESS]

Context:
- User impact: [WHY_THIS_MATTERS]
- Technical considerations: [CONSTRAINTS_OR_OPPORTUNITIES]
- Related components: [WHAT_MIGHT_BE_AFFECTED]

Please provide implementation plan and code for enhancement.
```

## Refactoring Template
```
Refactor: [COMPONENT_TO_REFACTOR]

Current issues:
- [Problem 1] - [Why it's problematic]
- [Problem 2] - [Impact on maintainability]

Goals:
- [Improvement 1] - [Specific benefit]
- [Improvement 2] - [How to measure success]

Constraints:
- Must maintain: [EXISTING_BEHAVIOR_TO_PRESERVE]
- Cannot break: [INTEGRATION_POINTS]
- Timeline: [ANY_URGENCY_FACTORS]

Please provide refactored code with migration plan if needed.
```
```

---

## Quality Standards

### Code Quality Gates

**Pre-Implementation Checklist**
- [ ] Task clearly defined in todo.md
- [ ] Dependencies identified and available
- [ ] Success criteria specified
- [ ] Template prompt prepared

**Post-Implementation Checklist**  
- [ ] Code runs without syntax errors
- [ ] Tests pass (or test plan documented if complex)
- [ ] Basic error handling included
- [ ] Documentation/comments for complex logic
- [ ] current-status.md updated with outcome

### Communication Quality Standards

**ChatGPT Strategic Interactions**
- Always include current project context
- Ask specific questions rather than general "what should I do?"
- Provide concrete progress updates, not just "working on X"
- Request actionable next steps, not just analysis

**Claude Implementation Interactions**
- Use specific templates from worker-prompts.md
- Include all necessary context and constraints
- Request complete implementations, not partial sketches
- Ask for explanations of complex decisions

---

## Platform-Specific Best Practices

### ChatGPT Desktop App Usage

**Conversation Management**
- Create separate chats for different projects
- Use descriptive chat titles: "[PROJECT] - Strategic Planning"
- Pin important conversations for quick access
- Export key decisions to todo.md for persistence

**Prompt Optimization**
- Front-load context in each conversation turn
- Use bullet points for complex project updates
- Ask for prioritized lists rather than paragraphs
- Request specific recommendations, not general advice

### Claude Desktop App Usage

**Artifact Leveraging**
- Request code artifacts for all implementations
- Use artifacts for documentation and configuration files
- Export artifacts directly to clipboard for VS Code transfer
- Maintain artifact version history for complex features

**Code Quality Optimization**
- Always request comprehensive error handling
- Ask for inline documentation for complex logic
- Request test cases with implementation
- Specify code style preferences in initial context

---

## Troubleshooting Guide

### Common Issues and Solutions

#### **Context Loss Problems**

**Problem**: ChatGPT forgetting project details in longer conversations
**Solution**: Create project context summary in todo.md header, paste regularly

**Problem**: Claude implementing inconsistent patterns across sessions  
**Solution**: Include specific code examples in worker-prompts.md templates

#### **Coordination Issues**

**Problem**: Losing track of what's completed vs. in-progress
**Solution**: Update current-status.md immediately after each task completion

**Problem**: Todo.md becoming overwhelming with too many items
**Solution**: Weekly cleanup - archive completed, reorganize by priority

#### **Quality Issues**

**Problem**: Claude code not integrating properly with existing codebase
**Solution**: Include more context about existing patterns in prompts

**Problem**: Strategic planning too abstract, not actionable
**Solution**: Always ask ChatGPT for specific next steps, not just analysis

### Emergency Procedures

**If ChatGPT Desktop App Unavailable**
- Use web interface with project context document
- Increase frequency of todo.md updates for continuity
- Focus on tactical execution with Claude until restored

**If Claude Desktop App Unavailable**  
- Use web interface with saved prompts from worker-prompts.md
- Break larger tasks into smaller pieces for context management
- Increase manual review/testing of generated code

**If Both Platforms Down**
- Continue with manual development using current-status.md as guide
- Document decisions in current-status.md for later AI consultation
- Focus on tasks that don't require complex planning or code generation

---

## Metrics and Success Measurement

### Velocity Tracking

**Daily Metrics** (5-second assessment)
- Tasks completed vs. planned (aim for 80%+ consistency)
- Blockers encountered and resolution time
- Code quality issues requiring rework

**Weekly Metrics** (5-minute review)
- Feature velocity: working features delivered per week
- Code stability: issues found after "completion"
- Planning accuracy: estimated vs. actual task completion time

### Quality Indicators

**Positive Signals**
- Most tasks complete without major rework
- Integration issues are rare and quickly resolved
- Daily planning feels accurate and achievable
- Code generated needs minimal modification

**Warning Signals**
- Frequent rework or "false completions"
- Growing backlog of partially finished features  
- Integration problems requiring architectural changes
- Difficulty estimating task complexity

---

## Appendix

### A. Initial File Templates

#### **current-status.md Template**
```markdown
# Project Status: [PROJECT_NAME]

## Sprint Goal: [DEFINE_WEEKLY_OBJECTIVE]
**Start Date**: [DATE]
**Target Completion**: [DATE]

## Daily Progress

### [TODAY_DATE] - [DAY_OF_WEEK]
**Focus**: [PLANNED_TASKS_FOR_TODAY]
**Completed**: 
- ✅ [Task completed] - [Brief outcome/notes]

**In Progress**:
- 🔄 [Task in progress] - [Current state]

**Issues/Blockers**:
- ⚠️ [None today / Description if any]

**Tomorrow's Priority**: [1-2_SPECIFIC_TASKS]

---

## Weekly Summary
**Velocity**: [To be updated at week end]
**Quality**: [Issues encountered, lessons learned]
**Next Week Focus**: [High-level objectives]
```

#### **todo.md Template**
```markdown
# Todo: [PROJECT_NAME]

## This Week (Priority Order)
- [ ] **HIGH**: [Define first high-priority task]
- [ ] **MEDIUM**: [Define medium-priority task]
- [ ] **LOW**: [Define nice-to-have task]

## Next Week
- [ ] [Plan ahead tasks]

## Backlog (Future Weeks)
- [ ] [Larger features or improvements]

## Blocked/Waiting
- [ ] [Tasks waiting on external factors]

## Done (This Week)
- [Completed tasks will move here]

## Ideas/Maybe
- [ ] [Future enhancement ideas]
```

#### **worker-prompts.md Template**
```markdown
# Worker Prompt Templates for [PROJECT_NAME]

## Standard Implementation Template
[Copy the template from the main document]

## Project-Specific Context
**Tech Stack**: [YOUR_SPECIFIC_TECHNOLOGIES]
**Code Patterns**: [YOUR_PREFERRED_PATTERNS]
**Testing Approach**: [HOW_YOU_HANDLE_TESTS]
**Documentation Style**: [YOUR_DOCUMENTATION_PREFERENCES]

## Common Task Types for This Project
[Customize based on your specific project needs]
```

### B. Desktop App Setup Tips

#### **ChatGPT Desktop Configuration**
- Enable keyboard shortcuts for quick access
- Set up conversation templates for different project types
- Configure notification preferences for focus management
- Use search function to quickly find previous project discussions

#### **Claude Desktop Configuration**  
- Set up quick artifact export workflows
- Configure editor preferences for code formatting
- Set up file type associations for easy artifact handling
- Enable/disable features based on workflow preferences

### C. Integration Scripts (Optional)

Simple bash scripts for common operations:

```bash
#!/bin/bash
# daily-status.sh - Quick status update helper
echo "## $(date '+%Y-%m-%d') - $(date '+%A')" >> current-status.md
echo "**Focus**: " >> current-status.md
echo "**Completed**: " >> current-status.md
echo "**Tomorrow's Priority**: " >> current-status.md
echo "" >> current-status.md
```

---

## Document Maintenance

**Review Triggers**
- End of each project (lessons learned integration)
- Monthly assessment of productivity and satisfaction
- When adding new tools or platforms to workflow
- After any major process improvements or issues

**Success Criteria for This SOP**
- Consistent daily progress on development projects
- Reduced context switching and decision fatigue
- Higher code quality with less rework
- Clear project visibility and predictable completion

**Version History**
- v2.0: Solo developer focus with file-based coordination
- v1.0: Enterprise multi-agent workflow (deprecated for solo use)

---

**Document Owner**: Solo Developer  
**Next Review**: [30 days from implementation]