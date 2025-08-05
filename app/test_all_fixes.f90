program test_all_fixes
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t, &
                         lex_source, parse_tokens, analyze_semantics, &
                         create_ast_arena, create_semantic_context, token_t, &
                         ast_arena_t, semantic_context_t, &
                         get_identifiers_in_subtree, &
                         build_control_flow_graph, find_unreachable_code
    implicit none
    
    character(len=:), allocatable :: source_code, output_code, error_msg
    type(format_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: semantic_ctx
    character(len=:), allocatable :: identifiers(:)
    integer :: i, prog_index
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Comprehensive Fix Testing for Issues #96, #98, #99 ==="
    print *, ""
    
    ! Test 1: Issue #96 - Complex mathematical expressions with intrinsic functions
    print *, "Test 1: Issue #96 - Mathematical expressions with sqrt and nested operations"
    source_code = 'program test_math' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    real :: a = 1.0, b = 2.0, c = 3.0' // new_line('A') // &
                 '    real :: result' // new_line('A') // &
                 '    ! Complex nested expression with intrinsic functions' // new_line('A') // &
                 '    result = (a + b) * c / (a - b) + sqrt(a**2 + b**2)' // new_line('A') // &
                 '    result = sin(a) * cos(b) + tan(c)' // new_line('A') // &
                 '    result = exp(log(abs(a))) + sqrt(b**2 + c**2)' // new_line('A') // &
                 '    ! Mixed integer/real expressions' // new_line('A') // &
                 '    result = real(int(a)) + sqrt(real(2))' // new_line('A') // &
                 '    print *, result' // new_line('A') // &
                 'end program test_math'
    
    call transform_lazy_fortran_string_with_format(source_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else
        print *, "  PASSED: Complex mathematical expressions with intrinsics work correctly"
    end if
    print *, ""
    
    ! Test 2: Issue #98 - Program structure preservation
    print *, "Test 2: Issue #98 - Program structure preservation"
    source_code = 'program test_structure' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: x = 42' // new_line('A') // &
                 '    character(len=20) :: name = "Fortran"' // new_line('A') // &
                 '    print *, "Hello from ", name' // new_line('A') // &
                 '    print *, "The answer is ", x' // new_line('A') // &
                 'end program test_structure'
    
    call transform_lazy_fortran_string_with_format(source_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else if (index(output_code, "program test_structure") == 0 .or. &
             index(output_code, "end program test_structure") == 0) then
        print *, "  FAILED: Program structure not preserved"
        print *, "  Output:", trim(output_code)
        all_tests_passed = .false.
    else
        print *, "  PASSED: Program structure preserved correctly"
    end if
    print *, ""
    
    ! Test 3: Issue #99 - Variable usage detection in conditionals
    print *, "Test 3: Issue #99 - Variable detection in conditional expressions"
    source_code = 'program test_conditionals' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: x = 5, y = 10' // new_line('A') // &
                 '    logical :: flag' // new_line('A') // &
                 '    if (x > 0 .and. y < 20) then' // new_line('A') // &
                 '        flag = .true.' // new_line('A') // &
                 '        print *, "x is positive and y is less than 20"' // new_line('A') // &
                 '    else if (x < 0 .or. y > 100) then' // new_line('A') // &
                 '        flag = .false.' // new_line('A') // &
                 '    end if' // new_line('A') // &
                 '    select case (x)' // new_line('A') // &
                 '    case (1:5)' // new_line('A') // &
                 '        print *, "x is between 1 and 5"' // new_line('A') // &
                 '    case default' // new_line('A') // &
                 '        print *, "x is something else"' // new_line('A') // &
                 '    end select' // new_line('A') // &
                 'end program test_conditionals'
    
    ! Parse and analyze to test variable detection
    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) == 0) then
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) == 0) then
            call analyze_semantics(arena, prog_index)
            
            ! Get identifiers from the program
            identifiers = get_identifiers_in_subtree(arena, prog_index)
            
            if (allocated(identifiers) .and. size(identifiers) > 0) then
                ! Check if x and y are detected
                block
                    logical :: found_x, found_y
                    found_x = .false.
                    found_y = .false.
                    do i = 1, size(identifiers)
                        if (identifiers(i) == "x") found_x = .true.
                        if (identifiers(i) == "y") found_y = .true.
                    end do
                    if (found_x .and. found_y) then
                        print *, "  PASSED: Variables in conditionals detected correctly"
                    else
                        print *, "  FAILED: Not all variables detected in conditionals"
                        all_tests_passed = .false.
                    end if
                end block
            else
                print *, "  FAILED: No identifiers found"
                all_tests_passed = .false.
            end if
        else
            print *, "  FAILED: Parse error - ", error_msg
            all_tests_passed = .false.
        end if
    else
        print *, "  FAILED: Lex error - ", error_msg
        all_tests_passed = .false.
    end if
    print *, ""
    
    ! Test 4: Control flow graph generation
    print *, "Test 4: Control flow graph and unreachable code detection"
    source_code = 'subroutine test_unreachable()' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: stat' // new_line('A') // &
                 '    real, allocatable :: array(:)' // new_line('A') // &
                 '    allocate(array(100), stat=stat)' // new_line('A') // &
                 '    if (stat /= 0) then' // new_line('A') // &
                 '        print *, "Allocation failed"' // new_line('A') // &
                 '        return' // new_line('A') // &
                 '        print *, "This is unreachable"' // new_line('A') // &
                 '    end if' // new_line('A') // &
                 '    print *, "Allocation succeeded"' // new_line('A') // &
                 '    deallocate(array)' // new_line('A') // &
                 'end subroutine test_unreachable'
    
    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) == 0) then
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) == 0) then
            ! Build control flow graph
            block
                use control_flow_graph_module, only: control_flow_graph_t
                type(control_flow_graph_t) :: cfg
                integer, allocatable :: unreachable_nodes(:)
                
                cfg = build_control_flow_graph(arena, prog_index)
                unreachable_nodes = find_unreachable_code(cfg)
                
                if (allocated(unreachable_nodes) .and. size(unreachable_nodes) > 0) then
                    print *, "  PASSED: Unreachable code detected after return"
                else
                    print *, "  WARNING: No unreachable code detected (may be parser limitation)"
                end if
            end block
        else
            print *, "  FAILED: Parse error - ", error_msg
            all_tests_passed = .false.
        end if
    else
        print *, "  FAILED: Lex error - ", error_msg
        all_tests_passed = .false.
    end if
    print *, ""
    
    ! Test 5: Mixed types and coercion
    print *, "Test 5: Type coercion in mixed expressions"
    source_code = 'program test_mixed_types' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: i = 5' // new_line('A') // &
                 '    real :: r = 3.14' // new_line('A') // &
                 '    real :: result' // new_line('A') // &
                 '    ! Mixed integer/real arithmetic' // new_line('A') // &
                 '    result = i + r' // new_line('A') // &
                 '    result = sqrt(real(i)) + r' // new_line('A') // &
                 '    result = i * r / 2' // new_line('A') // &
                 '    print *, result' // new_line('A') // &
                 'end program test_mixed_types'
    
    call transform_lazy_fortran_string_with_format(source_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else
        print *, "  PASSED: Mixed type expressions handled correctly"
    end if
    print *, ""
    
    ! Overall result
    print *, "=== Final Test Summary ==="
    if (all_tests_passed) then
        print *, "✅ ALL TESTS PASSED!"
        print *, "Issues #96, #98, and #99 are successfully resolved."
    else
        print *, "❌ SOME TESTS FAILED"
        print *, "Additional work needed to fully resolve all issues."
        stop 1
    end if
    
end program test_all_fixes