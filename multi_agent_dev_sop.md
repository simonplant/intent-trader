# Solo Developer Multi-Agent Workflow: Enterprise SOP

## Document Overview

**Purpose**: Define standard operating procedures for solo developer productivity using coordinated AI assistance across multiple platforms.

**Scope**: Individual software development projects requiring strategic planning, tactical execution, and continuous progress tracking.

**Version**: 2.0 - Solo Developer Edition  
**Last Updated**: May 2025  
**Review Cycle**: Monthly or per project completion

---

## Executive Summary

This SOP documents a proven solo developer workflow that leverages platform-specific AI strengths to maximize individual productivity. The methodology uses ChatGPT for strategic planning and project oversight, Claude for focused implementation work, and lightweight file-based tracking for continuous coordination.

**Key Benefits**:
- **3x Development Velocity**: Strategic planning reduces implementation rework
- **Consistent Quality**: Template-driven prompts ensure reliable output
- **Zero Overhead**: File-based tracking with no external tools required
- **Context Preservation**: Simple status files maintain project continuity

**Total Monthly Cost**: ~$120 (ChatGPT Pro $20 + Claude Pro $100)

---

## System Architecture

### Platform Assignments

#### **ChatGPT Pro ($20/month) + macOS Desktop App (Combined PM/Controller)**
- **Role**: Project Manager & Development Controller (Combined)
- **Strengths**: Strategic planning, task breakdown, progress coordination
- **Usage**: Architecture planning, task management, worker coordination, status integration
- **Data Access**: Repository zip files and INSTALL documentation

#### **Claude Pro ($100/month) + macOS Desktop App (Implementation Worker)**  
- **Role**: Senior Developer & Code Specialist
- **Strengths**: Superior code generation, detailed implementation, quality assurance
- **Usage**: Feature implementation, debugging, testing, technical documentation
- **Data Access**: GitHub repository sync for full codebase context

#### **Storage & Version Control Infrastructure**
- **Local Repository**: ~/Documents/code/intent-trader (iCloud-synced, git-enabled)
- **Remote Repository**: GitHub private repository (intent-trader)
- **Coordination Files**: current-status.md, todo.md, worker-prompts.md in repo root

### Information Flow

```
Project Manager/Controller (ChatGPT) ←→ Worker (Claude)
        ↓                                    ↓
    Updates todo.md                  Implements & reports
    Issues work prompts              Creates status summaries
    Integrates worker updates        Awaits feedback/rework
        ↓                                    ↓
    GitHub Repository ←→ iCloud Storage ←→ Local Development
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

**Step 1: Project Manager/Controller Setup (ChatGPT Desktop)**
```markdown
Combined PM/Controller Prompt:
"I'm starting a new solo development project called [PROJECT_NAME]. I need you to act as both Project Manager and Development Controller.

Project Details:
- Description: [PROJECT_DESCRIPTION]
- Technology: [TECH_STACK]
- Repository: ~/Documents/code/intent-trader (local) + GitHub private repo
- Timeline: [ROUGH_TIMELINE]
- Success Criteria: [WHAT_DEFINES_DONE]

Your dual responsibilities:
PROJECT MANAGER:
- Break project into logical phases and milestones
- Track progress and adjust timeline
- Identify risks and dependencies

DEVELOPMENT CONTROLLER:
- Create specific work tasks for Claude implementation
- Issue worker prompts with clear success criteria
- Integrate worker status reports and update todo.md
- Coordinate between architectural vision and tactical execution

I'll provide you with:
- Repository zip files when you need codebase context
- INSTALL files with setup/architecture documentation
- Worker status reports from Claude for integration

You'll help me:
- Plan daily priorities and task sequences
- Create specific worker prompts for Claude
- Track progress and update project status
- Make architectural decisions and adjust scope

Ready to start?"
```

**Step 2: Worker Setup (Claude Desktop with GitHub Sync)**
```markdown
Senior Developer Worker Prompt:
"I'm the implementation specialist for [PROJECT_NAME] - [BRIEF_DESCRIPTION].

Tech Stack: [TECHNOLOGIES]
Repository: Connected via GitHub sync to intent-trader private repo
Role: Senior developer responsible for implementation, testing, and quality

My responsibilities:
- Implement specific tasks issued by the Project Manager/Controller
- Write comprehensive, production-quality code
- Create thorough tests and documentation
- Perform quality assurance on all outputs
- Provide detailed status summaries for project coordination
- Respond to rework requests and refinements

Workflow:
1. Receive specific task prompts with success criteria
2. Implement complete solutions with tests
3. Generate mid-detail status summary for PM/Controller
4. Await feedback, rework requests, or next tasks

Code Standards: [SPECIFIC_PATTERNS]
Testing Approach: [TESTING_REQUIREMENTS]
Documentation Level: [DOC_REQUIREMENTS]

Confirm understanding and readiness for implementation tasks."
```

**Step 3: Repository and Storage Setup**
```bash
# Local repository setup
cd ~/Documents/code/
git clone git@github.com:username/intent-trader.git
cd intent-trader

# Create coordination files
touch current-status.md
touch todo.md  
touch worker-prompts.md
touch INSTALL.md

# Initial commit
git add .
git commit -m "Initial project setup with coordination files"
git push origin main

# Verify iCloud sync
# Files should appear in iCloud Drive/Documents/code/intent-trader
```

**Step 4: Platform Integration Setup**
```markdown
Claude GitHub Integration:
1. Connect Claude to intent-trader GitHub repository
2. Enable file sync for real-time codebase access
3. Test sync by making a small change and verifying Claude sees it

ChatGPT Data Access:
1. Create project zip file: zip -r intent-trader.zip ~/Documents/code/intent-trader
2. Drag zip file into ChatGPT chat for codebase context
3. Upload INSTALL.md for architecture and setup documentation
4. Update zip and re-upload when major changes occur
```

#### **Deliverables**
- [ ] ChatGPT context established as combined PM/Controller
- [ ] Claude context established as senior developer worker
- [ ] Local git repository created and synced to iCloud
- [ ] GitHub private repository created and connected to Claude
- [ ] ChatGPT has initial project zip and INSTALL documentation
- [ ] Coordination files created and initialized
- [ ] Initial todo.md populated with project breakdown

### SOP-002: Daily Development Workflow

#### **Morning Planning Ritual (5 minutes)**

**Step 1: Morning Planning & Task Assignment (ChatGPT Desktop)**
```markdown
Daily PM/Controller Prompt:
"Here's yesterday's status from current-status.md:
[PASTE_CURRENT_STATUS]

And here's my current todo.md:
[PASTE_CURRENT_TODOS]

As Project Manager/Controller, please:
1. Assess yesterday's progress against weekly goals
2. Identify 1-3 priority tasks for today  
3. Create specific worker prompts for Claude implementation
4. Flag any blockers, dependencies, or architectural decisions needed
5. Update my understanding of project timeline and risks

For each implementation task, provide:
- Clear task description with success criteria
- Context about how it fits into larger architecture
- Specific files/components to work on
- Definition of done with testing requirements"
```

**Step 2: Task Preparation**
- Select priority tasks from ChatGPT's recommendations
- Copy worker prompts into worker-prompts.md for reference
- Update todo.md with today's focus areas
- Prepare any additional context Claude might need

#### **Implementation Cycle (Variable Duration)**

**Step 3: Worker Task Execution (Claude Desktop)**
```markdown
Implementation Worker Task:
[PASTE_SPECIFIC_WORKER_PROMPT_FROM_CHATGPT]

Additional Context:
- Current codebase state: [Available via GitHub sync]
- Previous task outcomes: [IF_APPLICABLE]
- Integration points: [WHAT_THIS_CONNECTS_TO]

Please provide:
1. Complete implementation with comprehensive tests
2. Documentation for complex logic
3. Quality assurance notes
4. Mid-detail status summary for PM/Controller integration

Status summary format:
- Task: [WHAT_WAS_IMPLEMENTED]
- Status: [COMPLETED/PARTIAL/BLOCKED]
- Files modified: [LIST_OF_FILES]
- Testing: [TESTS_WRITTEN_AND_RESULTS]
- Integration notes: [DEPENDENCIES_OR_CONNECTIONS]
- Issues encountered: [ANY_PROBLEMS_OR_DECISIONS]
- Next steps: [RECOMMENDED_FOLLOW_UP_TASKS]
```

**Step 4: Integration & Quality Validation**
- Review Claude's implementation and status summary
- Test functionality in local development environment
- Commit changes to git with descriptive messages
- Push to GitHub (automatically syncs with Claude)
- Update current-status.md with completion details

**Step 5: Status Integration (ChatGPT Desktop)**
```markdown
Worker Status Integration Prompt:
"Claude completed a task and provided this status summary:
[PASTE_CLAUDE_STATUS_SUMMARY]

As PM/Controller, please:
1. Update our todo.md status based on this work
2. Identify any follow-up tasks or dependencies now unblocked
3. Assess impact on weekly goals and timeline
4. Determine next priority task or if rework is needed
5. Flag any architectural or strategic decisions needed

If the work is complete, suggest the next logical task.
If rework is needed, provide specific feedback for Claude."
```

**Step 6: Continuous Coordination**
- Update todo.md based on ChatGPT's assessment
- Prepare next worker prompt if continuing development
- Address any architectural questions or blockers
- Maintain momentum with clear next steps

#### **Evening Review (5 minutes)**

**Step 7: Daily Integration Review (ChatGPT Desktop)**
```markdown
End of Day PM/Controller Review:
"Here's today's completed work and status updates:
[PASTE_TODAY_STATUS_UPDATES]

As Project Manager/Controller, please:
1. Assess today's velocity against weekly sprint goals
2. Update project timeline and risk assessment
3. Identify tomorrow's priority tasks and sequence
4. Flag any strategic decisions or architectural reviews needed
5. Prepare worker prompts for tomorrow's priority tasks

Focus on maintaining momentum while ensuring quality and architectural integrity."
```

**Step 8: Daily Wrap-up**
- Update current-status.md with day's outcomes
- Commit coordination file updates to git
- Prepare tomorrow's focus areas in todo.md
- Ensure GitHub sync is current for Claude's next session

### SOP-003: Weekly Planning Cycle

#### **Weekly Review Process (30 minutes)**

**Step 1: Weekly Progress Assessment (ChatGPT PM/Controller)**
```markdown
Weekly PM/Controller Review:
"Here's this week's current-status.md entries and worker status summaries:
[PASTE_WEEK_STATUS]

Here's the updated todo.md:
[PASTE_CURRENT_TODOS]

As Project Manager/Controller, provide comprehensive weekly assessment:

PROJECT MANAGEMENT PERSPECTIVE:
1. Sprint velocity: Tasks completed vs. planned
2. Quality trends: Rework rates, integration issues
3. Timeline assessment: On track for milestones?
4. Risk evaluation: Emerging blockers or dependencies

DEVELOPMENT CONTROLLER PERSPECTIVE:  
1. Worker (Claude) performance: Task completion quality
2. Architecture evolution: Major decisions made this week
3. Technical debt: Issues that need addressing
4. Coordination effectiveness: Handoff quality, communication clarity

NEXT WEEK PLANNING:
1. Priority tasks and architectural focuses
2. Worker prompt improvements based on this week's experience
3. Process refinements for better PM/Controller → Worker coordination
4. Repository maintenance and documentation updates"
```

**Step 2: Next Week Coordination Setup**
- Update ChatGPT with any major architectural changes
- Refresh Claude context if significant patterns emerged
- Update worker-prompts.md with improved templates
- Create new project zip for ChatGPT if major repo changes occurred

**Step 3: Repository Maintenance**
- Clean up todo.md by archiving completed work
- Update INSTALL.md with any new setup requirements
- Ensure GitHub sync is functioning properly
- Backup coordination files and key decisions

---

### Repository Structure and File Management

#### **Local Development Environment**
```
~/Documents/code/intent-trader/          # iCloud-synced, git-enabled
├── current-status.md                    # Daily progress tracking
├── todo.md                              # Task backlog and priorities  
├── worker-prompts.md                    # Template library for Claude
├── INSTALL.md                           # Architecture and setup docs
├── README.md                            # Project overview
├── .git/                                # Git version control
├── src/                                 # Source code
├── tests/                               # Test files
├── docs/                                # Documentation
└── .gitignore                           # Git ignore patterns
```

#### **Storage and Sync Architecture**
- **iCloud Drive**: Automatic backup and cross-device sync
- **Local Git**: Version control and change tracking
- **GitHub Private**: Remote backup and Claude integration
- **Coordination Files**: Always in repo root for easy access

#### **Data Access Patterns**
- **Claude**: Real-time via GitHub repository sync
- **ChatGPT**: Periodic via zip file uploads and INSTALL.md
- **Developer**: Direct file system access via ~/Documents/code/intent-trader

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

### ChatGPT Desktop App (PM/Controller) Usage

**Context Management Strategy**
- Maintain single chat per project for continuity
- Use descriptive chat titles: "[PROJECT] - PM/Controller"
- Upload fresh project zip weekly or after major changes
- Keep INSTALL.md updated with current architecture

**Effective PM/Controller Prompting**
- Always specify both PM and Controller perspectives in requests
- Include current project state from coordination files
- Request specific worker prompts, not general guidance
- Ask for concrete next steps and priority sequences

### Claude Desktop App (Senior Worker) Usage

**GitHub Integration Optimization**
- Verify repository sync before starting work sessions
- Commit frequently to keep sync current
- Use clear commit messages for better context tracking
- Monitor sync status to ensure Claude sees latest changes

**Implementation Excellence Standards**
- Always provide complete, production-ready implementations
- Include comprehensive testing with each delivery
- Generate detailed status summaries for PM/Controller integration
- Proactively identify and communicate technical dependencies

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