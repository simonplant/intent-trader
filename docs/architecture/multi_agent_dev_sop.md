# Multi-Agent Development Workflow: Standard Operating Procedures

## Document Overview

**Purpose**: Define standard operating procedures for distributed AI-assisted development using multiple LLM platforms in coordinated roles.

**Scope**: Software development projects requiring complex orchestration, iterative implementation, and continuous state management.

**Version**: 1.0  
**Last Updated**: January 2025  
**Review Cycle**: Monthly

---

## Executive Summary

This SOP documents a proven multi-agent development workflow that leverages platform-specific strengths to create an efficient, scalable development process. The workflow utilizes ChatGPT for project management and task orchestration, Claude for focused implementation work, and human oversight for quality control and architectural decisions.

**Key Benefits**:
- **Platform Optimization**: Each AI platform used for its core strengths
- **Continuous State Management**: Project context maintained across sessions
- **Quality Assurance**: Human-in-the-loop validation at critical points
- **Scalable Coordination**: Can handle complex, multi-week development projects

---

## Workflow Architecture

### Role Definitions

#### **Project Manager (ChatGPT)**
- **Primary Responsibility**: Strategic oversight, progress tracking, roadmap management
- **Platform Rationale**: Unlimited context length for comprehensive project state
- **Key Activities**: Sprint planning, progress reporting, risk identification, stakeholder communication

#### **Controller (ChatGPT - Separate Chat)**
- **Primary Responsibility**: Task breakdown, dependency management, work queue coordination
- **Platform Rationale**: Complex task orchestration with full project context
- **Key Activities**: Work assignment, dependency tracking, integration planning, quality gate management

#### **Implementation Worker (Claude)**
- **Primary Responsibility**: Focused code generation, technical implementation, artifact creation
- **Platform Rationale**: Superior code generation and structured output capabilities
- **Key Activities**: Feature implementation, test creation, documentation generation, status reporting

#### **Integration Lead (Human Developer)**
- **Primary Responsibility**: Quality control, architectural decisions, final integration
- **Key Activities**: Code review, VS Code integration, GitHub management, architectural guidance

### Communication Flow

```
Project Manager → Controller → Implementation Worker → Integration Lead
     ↑                                                        ↓
     ←← Status Reports ←← Mid-Level Reports ←← Code + Status ←←
```

---

## Standard Operating Procedures

### SOP-001: Project Initialization

#### **Prerequisites**
- [ ] Project requirements documented
- [ ] Repository structure defined
- [ ] Development environment configured
- [ ] Platform access verified (ChatGPT, Claude, GitHub)

#### **Initialization Steps**

**Step 1: Project Manager Setup (ChatGPT)**
```markdown
Context Prompt:
"I need you to act as Project Manager for [PROJECT_NAME]. Your responsibilities include:
- Maintaining project roadmap and milestones
- Tracking overall progress and velocity
- Identifying risks and blockers
- Generating stakeholder reports

Project Overview: [DESCRIPTION]
Timeline: [DURATION]
Key Milestones: [LIST]
Success Criteria: [CRITERIA]

Please create initial project dashboard and confirm understanding."
```

**Step 2: Controller Setup (ChatGPT - New Chat)**
```markdown
Context Prompt:
"I need you to act as Development Controller for [PROJECT_NAME]. Your responsibilities include:
- Breaking down high-level tasks into implementable work items
- Managing dependencies and work queue
- Coordinating with implementation workers
- Tracking task completion and quality gates

Project Technical Stack: [TECH_DETAILS]
Repository Structure: [REPO_STRUCTURE]
Development Standards: [CODING_STANDARDS]

Please confirm role understanding and request first work breakdown."
```

**Step 3: Implementation Worker Templates (Claude)**
```markdown
Standard Worker Prompt Template:
"I am implementing [COMPONENT_NAME] for [PROJECT_NAME].

Task Details:
- Component: [COMPONENT_NAME]
- Dependencies: [LIST_FILES]
- Success Criteria: [CRITERIA]
- Technical Constraints: [CONSTRAINTS]

Please implement the component with:
1. Full implementation code
2. Comprehensive tests
3. Documentation
4. Status report for handoff

Current project context: [CONTEXT_SUMMARY]"
```

#### **Deliverables**
- [ ] Project Manager dashboard initialized
- [ ] Controller work queue established
- [ ] Implementation worker prompt templates ready
- [ ] Initial task breakdown completed

### SOP-002: Daily Development Cycle

#### **Morning Standup Process**

**Step 1: Project Manager Status Review (5 minutes)**
```markdown
Prompt: "Generate daily standup report including:
- Yesterday's completed milestones
- Today's priorities
- Current risks and blockers
- Overall project health (Green/Yellow/Red)
- Velocity metrics"
```

**Step 2: Controller Work Planning (10 minutes)**
```markdown
Prompt: "Based on current project status, plan today's work:
- Review completed tasks from yesterday
- Identify next priority tasks
- Check dependency readiness
- Assign work to implementation resources
- Flag any integration points"
```

**Step 3: Implementation Task Assignment (Variable)**
- Controller creates specific task prompts
- Implementation workers receive bounded, specific assignments
- Clear success criteria and deliverables defined

#### **Work Execution Cycle**

**Phase 1: Task Assignment**
```yaml
# Standard Task Assignment Format
task_id: "[PREFIX]-[NUMBER]-[DESCRIPTION]"
priority: "[HIGH|MEDIUM|LOW]"
estimated_effort: "[TIME_ESTIMATE]"
dependencies:
  required_files: ["file1.py", "file2.json"]
  completed_tasks: ["TASK-001", "TASK-002"]
context_summary: |
  Brief description of what's been built so far
  and how this task fits into the larger system
success_criteria:
  - "Specific measurable outcome 1"
  - "Specific measurable outcome 2"
deliverables:
  - "primary_file.py"
  - "test_file.py"
  - "status_report.md"
technical_constraints:
  - "Must follow existing schema patterns"
  - "Include comprehensive error handling"
```

**Phase 2: Implementation Execution (Claude)**
```markdown
Standard Implementation Process:
1. Analyze task requirements and dependencies
2. Review provided context and constraints
3. Generate implementation following project patterns
4. Create comprehensive tests
5. Generate status report using standard template
6. Highlight any issues or blockers identified
```

**Phase 3: Integration and Quality Control**
```markdown
Integration Checklist:
- [ ] Code syntax validates in VS Code
- [ ] Tests execute successfully
- [ ] Follows project coding standards
- [ ] Dependencies properly handled
- [ ] Documentation updated
- [ ] Status report complete and accurate
```

**Phase 4: Status Reporting**
```markdown
# Standard Status Report Template
## Task: [TASK_ID] - [DESCRIPTION]
## Status: [COMPLETED|IN_PROGRESS|BLOCKED|NEEDS_REVIEW]

### Deliverables Status:
- ✅ [file1.py] - Core functionality implemented
- ✅ [test_file.py] - Unit tests passing
- ⚠️ [integration_test.py] - Pending dependency completion

### Implementation Summary:
[Brief description of what was built and key decisions made]

### Technical Decisions:
- [Decision 1]: [Rationale]
- [Decision 2]: [Rationale]

### Dependencies for Next Steps:
- [dependency1.py] - Required for integration testing
- [dependency2.json] - Schema validation

### Issues/Blockers:
- [Issue description] - [Severity] - [Proposed resolution]

### Testing Status:
- Unit Tests: ✅ [X/Y] passing
- Integration Tests: ⏳ Pending [dependency]
- Performance Tests: ⏳ Not yet implemented

### Quality Metrics:
- Code Coverage: [X]%
- Documentation: [COMPLETE|PARTIAL|MISSING]
- Error Handling: [COMPREHENSIVE|BASIC|MISSING]

### Next Recommended Tasks:
1. [Next logical task based on current progress]
2. [Alternative task if blockers exist]
```

### SOP-003: Quality Gates and Validation

#### **Code Quality Gate**
```markdown
Quality Checklist (Before Task Completion):
- [ ] Code follows project naming conventions
- [ ] All functions have docstrings
- [ ] Error handling implemented for failure modes
- [ ] Input validation included where appropriate
- [ ] Tests cover happy path and edge cases
- [ ] No hardcoded values (use configuration)
- [ ] Logging included for debugging
- [ ] Performance considerations documented
```

#### **Integration Quality Gate**
```markdown
Integration Checklist:
- [ ] Dependencies properly imported
- [ ] File structure matches project conventions
- [ ] Tests execute in project environment
- [ ] No circular dependencies introduced
- [ ] Backward compatibility maintained
- [ ] Configuration changes documented
- [ ] Migration steps identified if needed
```

#### **Documentation Quality Gate**
```markdown
Documentation Checklist:
- [ ] README updated with new functionality
- [ ] API documentation generated/updated
- [ ] Configuration options documented
- [ ] Examples provided for complex features
- [ ] Troubleshooting guide updated
- [ ] Known limitations documented
```

### SOP-004: Progress Tracking and Reporting

#### **Daily Progress Update (End of Day)**

**Controller Status Update**
```markdown
Prompt: "Generate end-of-day status update:
- Tasks completed today with quality metrics
- Tasks in progress with completion estimates
- Blockers identified and resolution plans
- Tomorrow's priority queue
- Integration points ready for review
- Quality gate status for completed work"
```

**Project Manager Dashboard Update**
```markdown
Prompt: "Update project dashboard with today's progress:
- Milestone progress percentages
- Velocity metrics (tasks/day, story points)
- Risk register updates
- Resource utilization
- Schedule impacts from any delays
- Stakeholder communication needs"
```

#### **Weekly Sprint Review**

**Sprint Retrospective Process**
```markdown
Review Areas:
1. Velocity Analysis: Planned vs. actual task completion
2. Quality Metrics: Defect rates, rework requirements
3. Process Efficiency: Handoff delays, communication gaps
4. Platform Performance: Each role's effectiveness
5. Improvement Opportunities: Process refinements
```

**Sprint Planning Process**
```markdown
Planning Steps:
1. Review previous sprint completion
2. Assess current project state and risks
3. Define next sprint objectives
4. Break down objectives into specific tasks
5. Estimate effort and identify dependencies
6. Assign priorities and sequence work
7. Define success criteria and quality gates
```

### SOP-005: Issue Resolution and Escalation

#### **Blocker Resolution Process**

**Level 1: Implementation Worker (Claude)**
- Attempt alternative approaches
- Request additional context or clarification
- Identify specific technical blockers
- Escalate to Controller with detailed analysis

**Level 2: Controller (ChatGPT)**
- Analyze blocker impact on project schedule
- Evaluate alternative task sequencing
- Request additional resources or information
- Escalate to Project Manager for strategic decisions

**Level 3: Project Manager (ChatGPT)**
- Assess project-level impact
- Evaluate scope or timeline adjustments
- Communicate with stakeholders
- Escalate to Integration Lead for architectural decisions

**Level 4: Integration Lead (Human)**
- Make architectural or technology decisions
- Approve scope changes or timeline adjustments
- Provide additional context or resources
- Adjust project priorities or requirements

#### **Escalation Triggers**
- **Immediate**: Security vulnerabilities, data loss risks
- **Same Day**: Blockers affecting critical path tasks
- **Next Business Day**: Quality gate failures, dependency issues
- **Weekly Review**: Process inefficiencies, resource constraints

---

## Tools and Templates

### Required Tools

**Primary Platforms**
- **ChatGPT Plus**: For Project Manager and Controller roles
- **Claude Pro**: For implementation work and artifact generation
- **VS Code**: For code integration and validation
- **GitHub**: For version control and state persistence

**Recommended Extensions**
- **VS Code Extensions**: Python, JSON Schema validation, Git integration
- **Browser Extensions**: Session management for multiple chat contexts
- **Documentation Tools**: Markdown preview, diagram generation

### Template Library

#### **Project Initialization Templates**

**Project Charter Template**
```markdown
# Project Charter: [PROJECT_NAME]

## Project Overview
**Description**: [PROJECT_DESCRIPTION]
**Business Objective**: [BUSINESS_GOAL]
**Technical Objective**: [TECHNICAL_GOAL]

## Scope and Constraints
**In Scope**: [INCLUDED_FEATURES]
**Out of Scope**: [EXCLUDED_FEATURES]
**Technical Constraints**: [LIMITATIONS]

## Success Criteria
- [ ] [Measurable outcome 1]
- [ ] [Measurable outcome 2]
- [ ] [Measurable outcome 3]

## Timeline and Milestones
| Milestone | Date | Success Criteria |
|-----------|------|------------------|
| [Milestone 1] | [Date] | [Criteria] |
| [Milestone 2] | [Date] | [Criteria] |

## Risk Assessment
| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| [Risk 1] | [H/M/L] | [H/M/L] | [Strategy] |
```

**Task Breakdown Template**
```yaml
# Task: [TASK_ID]
title: "[DESCRIPTIVE_TITLE]"
priority: "[HIGH|MEDIUM|LOW]"
complexity: "[SIMPLE|MODERATE|COMPLEX]"
estimated_hours: [NUMBER]

dependencies:
  files: ["file1.py", "file2.json"]
  tasks: ["TASK-001", "TASK-002"]
  external: ["API_KEY", "CONFIG_SETTING"]

success_criteria:
  functional:
    - "Feature works as specified"
    - "All tests pass"
  quality:
    - "Code coverage > 80%"
    - "Documentation complete"
  integration:
    - "No breaking changes"
    - "Performance meets requirements"

deliverables:
  code: ["primary_file.py", "test_file.py"]
  documentation: ["README_update.md", "API_docs.md"]
  tests: ["unit_tests.py", "integration_tests.py"]

acceptance_criteria: |
  Detailed description of what constitutes successful completion,
  including specific behaviors, error handling, and edge cases.
```

### Communication Templates

#### **Daily Standup Template**
```markdown
# Daily Standup - [DATE]

## Overall Project Health: [🟢|🟡|🔴]

### Yesterday's Accomplishments
- [Task 1]: [Status] - [Brief description]
- [Task 2]: [Status] - [Brief description]

### Today's Priorities
1. [Priority 1] - [Assigned to] - [Expected completion]
2. [Priority 2] - [Assigned to] - [Expected completion]

### Blockers and Risks
- [Blocker 1]: [Description] - [Resolution plan]
- [Risk 1]: [Description] - [Mitigation strategy]

### Metrics
- **Velocity**: [Tasks completed/planned]
- **Quality**: [Tests passing/total]
- **Timeline**: [On track/Behind/Ahead]
```

#### **Handoff Communication Template**
```markdown
# Task Handoff: [TASK_ID]

## From: [SENDER_ROLE]
## To: [RECEIVER_ROLE]
## Date: [DATE]

### Task Summary
**What**: [Brief description of what was accomplished]
**Status**: [COMPLETED|NEEDS_REVIEW|BLOCKED]

### Deliverables
- ✅ [Delivered item 1] - [Location/description]
- ✅ [Delivered item 2] - [Location/description]
- ⚠️ [Partial item] - [What's missing and why]

### Context for Next Steps
[Explanation of current state and what the next person needs to know]

### Specific Next Actions Required
1. [Action 1] - [Why it's needed]
2. [Action 2] - [Dependencies or considerations]

### Issues or Concerns
- [Issue 1]: [Description and recommended approach]
- [Question 1]: [What needs clarification]

### Files Modified/Created
```
[file1.py] - [Description of changes]
[file2.py] - [New file, purpose]
[test_file.py] - [Test coverage details]
```

### Quality Checklist Completed
- [ ] Code review checklist completed
- [ ] Tests passing
- [ ] Documentation updated
- [ ] No known security issues
```

---

## Best Practices and Guidelines

### Communication Best Practices

#### **Prompt Engineering Guidelines**

**For Project Manager Interactions**
- Start with role context and project overview
- Include specific metrics and KPIs to track
- Request structured outputs for consistency
- Maintain project timeline and milestone focus

**For Controller Interactions**
- Provide current project state and dependencies
- Be specific about technical constraints
- Include quality criteria in all task assignments
- Request detailed work breakdown structures

**For Implementation Worker Interactions**
- Include all necessary context and dependencies
- Specify coding standards and patterns to follow
- Provide clear success criteria and deliverables
- Request status reports for handoff coordination

#### **Context Management**

**Maintaining Chat Context**
- Use consistent naming conventions for projects and tasks
- Include project state summaries in extended conversations
- Reference previous decisions and their rationales
- Maintain links to external documentation and repositories

**Cross-Platform State Synchronization**
- Update all relevant contexts when major changes occur
- Use structured status reports for consistent communication
- Maintain shared vocabulary and terminology
- Document decisions in accessible locations (GitHub wiki/docs)

### Quality Assurance Guidelines

#### **Code Quality Standards**

**Consistency Requirements**
- Follow established naming conventions
- Use consistent error handling patterns
- Maintain consistent documentation standards
- Apply consistent testing approaches

**Review Criteria**
- All functions have clear, single responsibilities
- Error handling covers expected failure modes
- Tests cover both happy path and edge cases
- Documentation explains not just what, but why

#### **Integration Quality Standards**

**Compatibility Requirements**
- Backward compatibility maintained unless explicitly planned
- Dependencies properly declared and managed
- Configuration changes documented and validated
- Migration paths identified for breaking changes

**Performance Considerations**
- Performance impact assessed for new features
- Resource usage monitored and optimized
- Scalability implications considered
- Load testing included for critical paths

### Risk Management

#### **Common Risk Patterns**

**Platform-Specific Risks**
- Context loss in ChatGPT due to conversation limits
- Claude artifacts not properly transferred to codebase
- Manual integration steps introducing errors
- Inconsistent quality across different implementation sessions

**Mitigation Strategies**
- Regular context refresh and state synchronization
- Standardized handoff procedures and checklists
- Automated validation where possible
- Cross-validation between platforms

#### **Quality Risk Management**

**Code Quality Risks**
- Inconsistent implementation across different workers
- Technical debt accumulation due to speed focus
- Inadequate test coverage for complex features
- Documentation falling behind implementation

**Process Risks**
- Task dependencies not properly managed
- Integration delays due to coordination issues
- Scope creep from inadequate initial planning
- Resource overcommitment leading to quality degradation

### Continuous Improvement

#### **Metrics Collection**

**Development Velocity Metrics**
- Tasks completed per day/week
- Time from task assignment to completion
- Rework frequency and causes
- Integration success rate

**Quality Metrics**
- Defect rates by component and implementer
- Test coverage and test success rates
- Documentation completeness scores
- User satisfaction with delivered features

#### **Process Optimization**

**Regular Review Cycles**
- Weekly retrospectives on process effectiveness
- Monthly review of role definitions and responsibilities
- Quarterly assessment of platform choices and alternatives
- Annual review of overall workflow architecture

**Improvement Implementation**
- Document lessons learned from each project
- Update templates and procedures based on experience
- Share best practices across project teams
- Invest in tooling and automation opportunities

---

## Troubleshooting Guide

### Common Issues and Resolutions

#### **Context Management Issues**

**Problem**: ChatGPT losing project context in long conversations
**Symptoms**: Inconsistent task assignments, forgetting project requirements
**Resolution**: 
- Create context refresh prompts with project summary
- Use structured state documents maintained in GitHub
- Consider starting new chats with full context for complex decisions

**Problem**: Claude implementing inconsistent patterns across sessions
**Symptoms**: Different coding styles, conflicting architectural approaches
**Resolution**:
- Include specific code examples in task prompts
- Maintain style guide and pattern library
- Cross-reference previous implementations in task context

#### **Coordination Issues**

**Problem**: Task dependencies not properly tracked
**Symptoms**: Implementation blocked by missing components, integration failures
**Resolution**:
- Use dependency tracking spreadsheet or project management tool
- Include dependency checks in task assignment process
- Implement daily dependency review in standup process

**Problem**: Quality variations between implementation sessions
**Symptoms**: Inconsistent test coverage, different error handling approaches
**Resolution**:
- Standardize quality checklists and validation criteria
- Include quality examples in implementation prompts
- Implement code review process with consistent criteria

#### **Integration Challenges**

**Problem**: Manual integration introducing errors
**Symptoms**: Syntax errors, missing imports, configuration issues
**Resolution**:
- Use automated syntax validation tools
- Create integration checklists for common issues
- Implement staged integration with validation at each step

**Problem**: Performance degradation with increased complexity
**Symptoms**: Slow response times, resource consumption issues
**Resolution**:
- Include performance considerations in task planning
- Implement performance testing and monitoring
- Design for scalability from initial implementation

---

## Appendix

### Reference Documentation

#### **Platform Documentation Links**
- [ChatGPT API Documentation](https://platform.openai.com/docs)
- [Claude API Documentation](https://docs.anthropic.com)
- [GitHub Actions Documentation](https://docs.github.com/actions)
- [VS Code Extension Development](https://code.visualstudio.com/api)

#### **Additional Resources**
- Multi-agent system design patterns
- Software development lifecycle best practices
- AI-assisted development methodologies
- Project management frameworks for technical projects

### Change Log

**Version 1.0 (January 2025)**
- Initial SOP documentation
- Core workflow definition
- Template library creation
- Best practices compilation

**Planned Updates**
- Automation tool integration
- Advanced metrics collection
- Platform alternative evaluation
- Scaling guidelines for larger teams

---

## Approval and Maintenance

**Document Owner**: [Project Lead Name]  
**Review Frequency**: Monthly  
**Next Review Date**: [Date + 1 Month]  
**Approval Required For**: Major process changes, tool additions, role redefinitions

**Change Request Process**:
1. Identify improvement opportunity or issue
2. Document proposed change with rationale
3. Test change with pilot project if significant
4. Update documentation and communicate changes
5. Monitor effectiveness and adjust as needed