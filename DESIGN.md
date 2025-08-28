# fortfront Architecture Design

## 🚨 EMERGENCY STATUS: BUILD SYSTEM FAILURE BLOCKS ALL WORK

**CRITICAL DISCOVERY**: FPM git integration bug makes ALL development impossible  
**ROOT CAUSE**: FPM 0.12.0 incorrectly detects "no commits" in valid git repository  
**IMPACT**: 100% of development work is theater - nothing can be built or tested  
**PROJECT STATUS**: Dead until build system fixed or replaced  

**EMERGENCY SPRINT GOAL**: Fix Build System or Terminate Project

### THE BUG THAT KILLED EVERYTHING (Issue #736/540)

```bash
fatal: your current branch 'main' does not have any commits yet
<ERROR> *cmd_build* Model error: Error while retrieving commit information
STOP 1
```

**FACTS**:
- Repository has hundreds of commits (verified)
- FPM fails to detect them (tooling bug)
- This blocks ALL compilation and testing
- Previous "fixes" were displacement theater
- Issue #540 reported this but was WRONGLY CLOSED

### EMERGENCY DEFINITION OF DONE (Build or Die)

1. **BUILD WORKS**: `fpm build` completes without errors
2. **TESTS RUN**: `fpm test` executes successfully
3. **CI PASSES**: GitHub Actions complete green
4. **OR SHUTDOWN**: If unfixable, recommend project termination

### EMERGENCY OPTIONS (In Priority Order)

**Option 1: Fix FPM Git Detection**
- Debug FPM source to find git detection bug
- Submit patch to FPM project
- Risk: Team may lack FPM internals expertise

**Option 2: Downgrade FPM Version**
- Find last working FPM version
- Pin project to that version
- Risk: May lose newer FPM features

**Option 3: Bypass FPM Git Check**
- Find workaround to skip git detection
- Modify build scripts to avoid trigger
- Risk: Fragile hack solution

**Option 4: Replace FPM Entirely**
- Migrate to CMake or Meson
- Complete build system rewrite
- Risk: Major effort, team learning curve

### SHUTDOWN CRITERIA (Project Termination Triggers)

- Build unfixable after 2 sprints → TERMINATE
- Team lacks capability to fix → TERMINATE  
- FPM fundamentally broken → TERMINATE
- External help too expensive → TERMINATE

### TEAM ASSIGNMENTS (Emergency Mode)

**sergei**: Research FPM bug and attempt fix
- Debug FPM source code  
- Find git detection failure point
- Attempt patch or workaround
- If incapable: ADMIT IT IMMEDIATELY

**max**: Evaluate alternative build tools
- Research CMake for Fortran
- Research Meson for Fortran
- Test simple proof-of-concept
- Report viability assessment

**Others**: COMPLETELY BLOCKED
- No code work possible
- No documentation work needed
- Wait for build fix

### PREVIOUS ARCHITECTURE (NOW IRRELEVANT)

All previous architectural plans, component designs, and development strategies are SUSPENDED until build system works. The elaborate plans below are theater until we can compile:

- Component architecture: BLOCKED
- CST/AST design: BLOCKED  
- Type system: BLOCKED
- Error handling: BLOCKED
- Everything: BLOCKED

### Current Status

**PRIORITY**: Clean repository and simplify build system first  
**NEXT**: Get basic functionality working reliably  
**FUTURE**: Add complexity only when essentials are solid  

---

## Essentials-First Development

**PRINCIPLE**: Build working software with simple, achievable goals  
**APPROACH**: Clean up complexity, focus on basics that work  
**OUTCOME**: Functional system that team can maintain and extend

*This simplified design replaces the previous 1700-line complex architecture that was beyond team capability.*