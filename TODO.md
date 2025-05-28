# 📋 Intent Trader - Comprehensive TODO

*Last Updated: December 2024*

## 🎯 **Project Overview**
Refactor intent-trader-mvc (unreliable) → Native IAA app for ChatGPT/Claude.ai
- **Target**: Stateless, context-driven, PFEMRC-based trading assistant
- **Optimization**: Lowest cost via MAX mode (minimal API calls, efficient tokens)
- **Architecture**: Chat-native IAA with embedded schemas and natural language intents

## ⚠️ **IMPORTANT: Code Change Rules**
*These rules must be followed for ALL code changes and deletions*

1. **NO Automatic Deletions**
   - All file deletions require explicit user approval
   - No bulk deletions without individual review
   - Preserve all code until explicitly approved for removal

2. **Safe Migration Process**
   - Move code to new locations before deletion
   - Keep both old and new implementations until verified
   - Document all changes before execution

3. **Change Approval Process**
   - Present all proposed changes for review
   - Wait for explicit approval before execution
   - Provide rollback plan for all changes

4. **Code Preservation**
   - Back up code before major changes
   - Keep historical versions accessible
   - Document all migrations and changes

5. **Git Safety Rules**
   - NO force operations (`git push --force`, `git reset --hard`, etc.)
   - NO destructive Git commands without explicit approval
   - Always create backup branches before major changes
   - Use `git stash` instead of force operations
   - Document all Git operations before execution

---

## 🚨 **PHASE 1: Critical Fixes (Week 1)**
*Priority: URGENT - Foundation must be solid*

### 1.1 Code Consolidation & Cleanup
- [ ] **Remove duplicate implementations**
  - [ ] Delete `execution/order_manager.py` (keep `src/order/manager.py`)
  - [ ] Delete `data/market_data_provider.py` (keep `src/market/feed.py`)
  - [ ] Remove duplicate directories: `execution/`, `data/`, `core/`, `risk/`, `agents/`, `brokers/`, `prompts/`, `actions/`
  - [ ] Consolidate all implementations under `src/`
- [ ] **Fix broken test infrastructure**
  - [ ] Fix `tests/conftest.py` import errors
  - [ ] Remove references to non-existent `data` module
  - [ ] Ensure all tests can discover and import `src/` modules
  - [ ] Verify `pytest --collect-only` works without errors
- [ ] **Clean up dependencies**
  - [ ] Remove `asyncio>=3.4.3` (built-in module)
  - [ ] Fix duplicate `SQLAlchemy>=2.0.0` and `sqlalchemy>=2.0.0`
  - [ ] Pin all package versions for reproducibility
  - [ ] Create clean `requirements-iaa.txt` for new architecture

### 1.2 Project Structure Reorganization
- [ ] **Create clean IAA structure**
  ```
  intent-trader-iaa/
  ├── src/
  │   ├── iaa_core.py          # Main IAA logic
  │   ├── phase_handlers.py    # PFEMRC implementations
  │   ├── context_manager.py   # Context extraction
  │   ├── intent_processor.py  # Natural language → actions
  │   └── utils/              # Shared utilities
  ├── prompts/                # Phase-specific prompts
  ├── tests/                  # Comprehensive test suite
  └── docs/                   # IAA-specific documentation
  ```
- [ ] **Migrate essential components**
  - [ ] Extract core logic from intent-trader-mvc
  - [ ] Adapt ConfigManager for IAA context
  - [ ] Simplify LogManager for chat environments
  - [ ] Create minimal DatabaseManager (if needed)

---

## 🏗️ **PHASE 2: IAA Core Framework (Week 2)**
*Priority: HIGH - Core functionality*

### 2.1 Context Management System
- [ ] **Implement ContextManager**
  - [ ] `extract_from_conversation()` - Parse chat history
  - [ ] `detect_current_phase()` - Identify PFEMRC phase
  - [ ] `extract_positions()` - Parse position data from text
  - [ ] `extract_trading_plan()` - Parse plan from conversation
  - [ ] `extract_session_metrics()` - Parse P&L, metrics
- [ ] **Context compression for cost optimization**
  - [ ] Compressed position format: `"AAPL:L225.50/223.80/227.50"`
  - [ ] Smart summarization for long inputs
  - [ ] Incremental context updates

### 2.2 Intent Processing Engine
- [ ] **Natural language intent recognition**
  - [ ] Replace slash commands with NL intents
  - [ ] Intent classification (PLAN/FOCUS/EXECUTE/MANAGE/REVIEW/COACH)
  - [ ] Parameter extraction from natural language
  - [ ] Intent validation and error handling
- [ ] **Response formatting for chat**
  - [ ] Emoji-based visual structure
  - [ ] Token-efficient responses
  - [ ] Context preservation in responses

### 2.3 PFEMRC Phase Handlers
- [ ] **PLAN Phase Handler**
  - [ ] Morning call analysis (compressed)
  - [ ] Level extraction
  - [ ] Market bias assessment
  - [ ] Key idea identification
- [ ] **FOCUS Phase Handler**
  - [ ] Trading plan creation
  - [ ] Setup prioritization
  - [ ] Risk assessment
  - [ ] Focus idea ranking
- [ ] **EXECUTE Phase Handler**
  - [ ] Position sizing calculations
  - [ ] Entry decision support
  - [ ] Order parameter generation
  - [ ] Risk validation
- [ ] **MANAGE Phase Handler**
  - [ ] Position tracking
  - [ ] Stop/target adjustments
  - [ ] P&L monitoring
  - [ ] Exit decision support
- [ ] **REVIEW Phase Handler**
  - [ ] Session performance analysis
  - [ ] Trade outcome recording
  - [ ] Learning extraction
  - [ ] Metrics calculation
- [ ] **COACH Phase Handler**
  - [ ] Performance feedback
  - [ ] Improvement suggestions
  - [ ] Pattern recognition
  - [ ] Behavioral insights

---

## 🔧 **PHASE 3: Enhanced Features (Week 3-4)**
*Priority: MEDIUM - Functionality expansion*

### 3.1 Advanced Context Features
- [ ] **Multi-session context**
  - [ ] Session history summarization
  - [ ] Performance trend tracking
  - [ ] Learning persistence
  - [ ] Pattern recognition across sessions
- [ ] **Smart context pruning**
  - [ ] Automatic context compression
  - [ ] Relevance-based filtering
  - [ ] Token budget management
  - [ ] Context refresh strategies

### 3.2 Enhanced Trading Logic
- [ ] **Advanced position management**
  - [ ] Portfolio-level risk tracking
  - [ ] Correlation analysis
  - [ ] Exposure monitoring
  - [ ] Dynamic position sizing
- [ ] **Improved strategy support**
  - [ ] Multiple strategy frameworks
  - [ ] Strategy performance tracking
  - [ ] Adaptive strategy selection
  - [ ] Custom strategy creation

### 3.3 Integration & Validation
- [ ] **Chat platform testing**
  - [ ] ChatGPT integration testing
  - [ ] Claude.ai integration testing
  - [ ] Response format optimization
  - [ ] Error handling in chat context
- [ ] **Performance optimization**
  - [ ] Token usage monitoring
  - [ ] Response time optimization
  - [ ] Memory usage optimization
  - [ ] Cost tracking and reporting

---

## 🧪 **PHASE 4: Testing & Quality (Ongoing)**
*Priority: HIGH - Quality assurance*

### 4.1 Comprehensive Testing
- [ ] **Unit tests for all components**
  - [ ] ContextManager tests
  - [ ] Phase handler tests
  - [ ] Intent processor tests
  - [ ] Utility function tests
- [ ] **Integration tests**
  - [ ] End-to-end PFEMRC flow tests
  - [ ] Context persistence tests
  - [ ] Multi-phase interaction tests
  - [ ] Error recovery tests
- [ ] **Chat environment tests**
  - [ ] Mock chat conversation tests
  - [ ] Context extraction accuracy tests
  - [ ] Response format validation
  - [ ] Token usage measurement

### 4.2 Performance & Security
- [ ] **Performance testing**
  - [ ] Response time benchmarks
  - [ ] Token usage analysis
  - [ ] Memory usage profiling
  - [ ] Concurrent usage testing
- [ ] **Security validation**
  - [ ] Input sanitization
  - [ ] API key protection
  - [ ] Data privacy compliance
  - [ ] Error information leakage prevention

---

## 📚 **PHASE 5: Documentation & Deployment (Week 5)**
*Priority: MEDIUM - User experience*

### 5.1 Documentation
- [ ] **User documentation**
  - [ ] IAA usage guide
  - [ ] PFEMRC workflow examples
  - [ ] Common use cases
  - [ ] Troubleshooting guide
- [ ] **Technical documentation**
  - [ ] Architecture overview
  - [ ] API reference
  - [ ] Development guide
  - [ ] Deployment instructions

### 5.2 Deployment Preparation
- [ ] **Package preparation**
  - [ ] Clean requirements.txt
  - [ ] Installation scripts
  - [ ] Configuration templates
  - [ ] Example conversations
- [ ] **Quality gates**
  - [ ] All tests passing
  - [ ] Documentation complete
  - [ ] Performance benchmarks met
  - [ ] Security review passed

---

## 🔄 **Migration from MVC (Parallel Track)**
*Priority: MEDIUM - Legacy system transition*

### Migration Tasks
- [ ] **Schema migration**
  - [ ] Convert JSON schemas to embedded prompts
  - [ ] Simplify data structures for chat
  - [ ] Create migration utilities
  - [ ] Validate data integrity
- [ ] **Command migration**
  - [ ] Map slash commands to natural intents
  - [ ] Create intent examples
  - [ ] Test command equivalency
  - [ ] Document migration guide
- [ ] **Data migration**
  - [ ] Export existing session data
  - [ ] Convert to new format
  - [ ] Import into new system
  - [ ] Validate migration accuracy

---

## 📊 **Success Metrics**

### Technical Metrics
- [ ] **Reliability**: 99%+ successful intent processing
- [ ] **Performance**: <2 second response time
- [ ] **Cost**: <50% token usage vs current MVC
- [ ] **Quality**: 95%+ test coverage

### User Experience Metrics
- [ ] **Usability**: Natural language intent success rate >90%
- [ ] **Accuracy**: Context extraction accuracy >95%
- [ ] **Efficiency**: Reduced interaction steps by 60%
- [ ] **Satisfaction**: User feedback score >4.5/5

---

## 🎯 **Current Sprint Focus**
*Update this section weekly*

### This Week's Priorities:
1. [ ] **URGENT**: Fix broken test infrastructure
2. [ ] **URGENT**: Remove duplicate code implementations
3. [ ] **HIGH**: Create basic IAA framework structure
4. [ ] **HIGH**: Implement ContextManager foundation

### Next Week's Targets:
1. [ ] Complete PLAN phase handler
2. [ ] Implement basic intent processing
3. [ ] Create first working PFEMRC flow
4. [ ] Begin chat platform testing

---

## 📝 **Notes & Decisions**

### Architecture Decisions
- **Stateless Design**: All state maintained in conversation context
- **Embedded Schemas**: No external schema files, prompts contain structure
- **Natural Language**: Replace slash commands with NL intents
- **Token Optimization**: Compressed formats, smart summarization

### Risk Mitigation
- **Parallel Development**: Keep MVC running while building IAA
- **Incremental Migration**: Phase-by-phase transition
- **Rollback Plan**: Ability to revert to MVC if needed
- **Testing Strategy**: Extensive testing before each phase

---

*This TODO will be updated weekly with progress and new insights.* 