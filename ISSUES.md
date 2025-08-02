# Prioritized GitHub Issues

This document lists all open GitHub issues prioritized by architectural impact and implementation complexity.

## ⚠️ Development Workflow Note

**All completed issues must await qodo merge feedback and code coverage analysis before being considered fully closed.** Issues marked as ✅ COMPLETED with ⚠️ PENDING status indicate implementation is done but awaiting final quality review.

## 🔴 Critical Architectural Issues (Blocking)

### ✅ #32 - Removal of get_node() breaks AST traversal - need accessor functions
**Priority: URGENT** | **Impact: Architectural** | **Effort: High**
- **Status**: ✅ **COMPLETED** - Comprehensive accessor functions implemented (PR #37)
- **Description**: Core API removal prevents accessing node-specific fields
- **Architectural Impact**: Requires fundamental API design decision
- **Dependencies**: ✅ Resolved - unblocks issues #33, #30, #29, #13
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #33 - Add symbol table and scope information to public API
**Priority: High** | **Impact: Architectural** | **Effort: High** 
- **Status**: ✅ **COMPLETED** - Comprehensive symbol table API implemented (PR #38)
- **Description**: Expose semantic analysis results for symbol resolution
- **Architectural Impact**: Major addition to public API design
- **Dependencies**: ✅ Depends on #32 resolution
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #11 - AST Arena Access Methods Are Stub Implementations  
**Priority: High** | **Impact: Architectural** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - All stub methods fully implemented
- **Labels**: bug, enhancement
- **Description**: Many arena methods are stubs, limiting functionality
- **Architectural Impact**: Core data structure incomplete
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

## 🟠 High-Impact Parser & AST Enhancements

### ✅ #35 - Add allocate/deallocate support to lazy Fortran parser
**Priority: High** | **Impact: Parser Architecture** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - allocate/deallocate keywords added to lexer, tests passing (PR #40)
- **Description**: Integrate existing allocate/deallocate parsing with lazy parser
- **Root Cause**: Missing lexer keyword recognition, not parser dispatcher integration
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #13 - Missing AST Traversal and Visitor Pattern Implementation
**Priority: High** | **Impact: Architectural** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - AST traversal and visitor pattern implemented (PR pending)
- **Labels**: enhancement
- **Description**: Core traversal patterns missing from API
- **Dependencies**: Related to #32
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #30 - Add intrinsic function identification and signature information
**Priority: Medium** | **Impact: AST Design** | **Effort: High**
- **Status**: ✅ **COMPLETED** - Intrinsic function identification and signature information implemented (PR #44)
- **Description**: Distinguish intrinsic functions from user functions in AST
- **Architectural Impact**: Changes function call representation
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #29 - Add WHERE and FORALL statement/construct detailed representation
**Priority: Medium** | **Impact: AST Design** | **Effort: High**
- **Status**: ✅ **COMPLETED** - WHERE and FORALL AST nodes implemented (PR #45)
- **Description**: Enhanced AST nodes for complex array constructs
- **Architectural Impact**: Significant AST node additions
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #28 - Add bounds information to array operations
**Priority: Medium** | **Impact: AST Design** | **Effort: High**
- **Status**: ✅ **COMPLETED** - Array bounds information added to AST nodes (PR #46)
- **Description**: Track array bounds for safety and optimization
- **Architectural Impact**: Fundamental for array analysis
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #27 - Add allocatable and pointer attribute propagation in expressions
**Priority: Medium** | **Impact: AST Design** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - Allocatable and pointer attribute propagation implemented (PR #47)
- **Description**: Track memory attributes through expressions
- **Architectural Impact**: Critical for memory management
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

## 🟡 Medium-Impact AST Node Improvements

### ✅ #22 - Add explicit array section/slice AST nodes
**Priority: Medium** | **Impact: AST Design** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - Array slice nodes already implemented, fixed stride parsing (PR #48)
- **Description**: Dedicated nodes for array slicing operations
- **Note**: Array slice infrastructure (array_slice_node, array_bounds_node) was already present; this PR fixed stride parsing
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #21 - Disambiguate array indexing from function calls in call_or_subscript_node
**Priority: Medium** | **Impact: AST Design** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - Added is_array_access flag to disambiguate array indexing from function calls (PR pending)
- **Description**: Separate array access from function calls in AST
- **Implementation**: Added boolean flag to call_or_subscript_node, set during semantic analysis
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #24 - Add dedicated component access AST node for % operator
**Priority: Medium** | **Impact: AST Design** | **Effort: Low**
- **Status**: ✅ **COMPLETED** - Dedicated component_access_node implemented (PR #50)
- **Description**: Specific node type for derived type component access
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #25 - Add character substring AST node support
**Priority: Medium** | **Impact: AST Design** | **Effort: Low**
- **Status**: ✅ **COMPLETED** - Character substring support implemented using range_subscript_node (PR #51)
- **Description**: Handle character substring operations
- **Implementation**: Created range_subscript_node for parse-time ambiguity, added is_character_substring flag
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

### ✅ #23 - Add ASSOCIATE construct AST node support
**Priority: Medium** | **Impact: AST Design** | **Effort: Medium**
- **Status**: ✅ **COMPLETED** - ASSOCIATE construct AST node implemented (PR #52)
- **Description**: Support ASSOCIATE construct parsing with full AST support
- **Implementation**: Created associate_node type with lexer, parser, codegen, and comprehensive test coverage
- **⚠️ PENDING**: Awaiting qodo merge feedback and code coverage analysis before final closure

## 🟢 API and Documentation Improvements

### 🚧 #34 - Document node type identification strategy in public API
**Priority: Medium** | **Impact: Documentation** | **Effort: Low**
- **Status**: 🚧 **IN PROGRESS** - Working on node type identification documentation
- **Labels**: documentation, enhancement
- **Description**: Document how to properly identify AST node types
- **Branch**: feature/34-document-node-type-identification

### 🚧 #14 - Missing Public API for Semantic Information Queries
**Priority: Medium** | **Impact: API Design** | **Effort: Medium**
- **Status**: 🚧 **IN PROGRESS** - Working on semantic information query API
- **Labels**: enhancement
- **Description**: Expose semantic analysis results through API
- **Branch**: feature/14-semantic-info-api

### #12 - Missing AST Node Introspection APIs for Static Analysis
**Priority: Medium** | **Impact: API Design** | **Effort: Medium**
- **Labels**: enhancement
- **Description**: APIs for examining AST node properties

## 🔵 Analysis and Optimization Features

### #26 - Add expression temporary tracking for optimization
**Priority: Low** | **Impact: Optimization** | **Effort: High**
- **Description**: Track temporary variables for memory optimization

### #20 - Include parameter attributes (optional, intent) in AST nodes
**Priority: Low** | **Impact: AST Enhancement** | **Effort: Low**
- **Labels**: enhancement
- **Description**: Add parameter metadata to AST nodes

### #19 - Need clearer block boundaries and sibling relationships in AST
**Priority: Low** | **Impact: API Enhancement** | **Effort: Medium**
- **Labels**: enhancement
- **Description**: Improve AST navigation capabilities

### #18 - Need call graph construction for unused procedure detection
**Priority: Low** | **Impact: Analysis Feature** | **Effort: High**
- **Labels**: enhancement
- **Description**: Build call graphs for static analysis

### #17 - Need control flow graph API for reachability analysis
**Priority: Low** | **Impact: Analysis Feature** | **Effort: High**
- **Labels**: enhancement
- **Description**: Provide control flow analysis capabilities

### #16 - Need API to track variable usage within expression subtrees
**Priority: Low** | **Impact: Analysis Feature** | **Effort: Medium**
- **Labels**: enhancement
- **Description**: Track variable usage for analysis tools

### #15 - Performance Issues with Large-Scale AST Processing
**Priority: Low** | **Impact: Performance** | **Effort: Medium**
- **Labels**: enhancement
- **Description**: Optimize AST processing for large codebases

## 🟣 Simple Fixes

### #36 - Parser does not handle .eqv. and .neqv. logical operators correctly
**Priority: Low** | **Impact: Parser Bug** | **Effort: Low**
- **Description**: Add support for equivalence operators
- **Status**: Tests disabled, implementation needed

---

## Implementation Strategy

1. **Phase 1**: Resolve critical architectural blockers (#32, #33, #11)
2. **Phase 2**: Implement high-impact parser enhancements (#35, #13)
3. **Phase 3**: Add major AST node improvements (#30, #29, #28, #27)
4. **Phase 4**: Complete medium-impact AST nodes (#22, #21, #24, #25, #23)
5. **Phase 5**: Improve API and documentation (#34, #14, #12)
6. **Phase 6**: Add analysis features (#26, #20, #19, #18, #17, #16, #15)
7. **Phase 7**: Fix simple bugs (#36)

## Dependencies

```
#32 (get_node) → blocks many others
#33 (symbol table) → depends on #32
#13 (visitor pattern) → related to #32  
#35 (allocate/deallocate) → independent
#11 (arena methods) → foundational
```

Total Issues: 14 open issues requiring attention.