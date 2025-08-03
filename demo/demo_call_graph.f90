program demo_call_graph
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: ctx
    integer :: root_index
    type(call_graph_t) :: graph
    character(len=:), allocatable :: unused(:), callers(:), callees(:)
    character(len=:), allocatable :: all_procs(:)
    type(call_edge_t), allocatable :: edges(:)
    integer :: i

    print *, "=== Call Graph Analysis Demo ==="
    print *

    ! Sample Fortran code with various procedure types
    source = "program main" // new_line('a') // &
            "implicit none" // new_line('a') // &
            "real :: x, y" // new_line('a') // &
            "x = 5.0" // new_line('a') // &
            "y = compute_result(x)" // new_line('a') // &
            "call print_result(y)" // new_line('a') // &
            "call analyze_data()" // new_line('a') // &
            "contains" // new_line('a') // &
            "" // new_line('a') // &
            "function compute_result(input) result(output)" // new_line('a') // &
            "    real :: input, output" // new_line('a') // &
            "    output = helper_function(input) * 2.0" // new_line('a') // &
            "    call log_computation(input, output)" // new_line('a') // &
            "end function compute_result" // new_line('a') // &
            "" // new_line('a') // &
            "function helper_function(val) result(res)" // new_line('a') // &
            "    real :: val, res" // new_line('a') // &
            "    res = val + 1.0" // new_line('a') // &
            "end function helper_function" // new_line('a') // &
            "" // new_line('a') // &
            "subroutine print_result(value)" // new_line('a') // &
            "    real :: value" // new_line('a') // &
            "    print *, 'Result:', value" // new_line('a') // &
            "end subroutine print_result" // new_line('a') // &
            "" // new_line('a') // &
            "subroutine log_computation(input, output)" // new_line('a') // &
            "    real :: input, output" // new_line('a') // &
            "    print *, 'Computed', output, 'from', input" // new_line('a') // &
            "end subroutine log_computation" // new_line('a') // &
            "" // new_line('a') // &
            "subroutine analyze_data()" // new_line('a') // &
            "    print *, 'Analyzing data...'" // new_line('a') // &
            "    call process_internal_data()" // new_line('a') // &
            "end subroutine analyze_data" // new_line('a') // &
            "" // new_line('a') // &
            "subroutine process_internal_data()" // new_line('a') // &
            "    print *, 'Processing internal data'" // new_line('a') // &
            "end subroutine process_internal_data" // new_line('a') // &
            "" // new_line('a') // &
            "! This subroutine is never called" // new_line('a') // &
            "subroutine unused_cleanup()" // new_line('a') // &
            "    print *, 'Cleaning up (never executed)'" // new_line('a') // &
            "end subroutine unused_cleanup" // new_line('a') // &
            "" // new_line('a') // &
            "! This function is also unused" // new_line('a') // &
            "function dead_code() result(x)" // new_line('a') // &
            "    real :: x" // new_line('a') // &
            "    x = 42.0" // new_line('a') // &
            "end function dead_code" // new_line('a') // &
            "" // new_line('a') // &
            "end program main"

    print *, "Sample Fortran Code:"
    print *, "==================="
    print *, source
    print *

    ! Lex, parse, and analyze
    print *, "Step 1: Lexical Analysis, Parsing, and Semantic Analysis"
    print *, "-------------------------------------------------------"
    call tokenize_core(source, tokens)
    arena = create_ast_arena()
    root_index = parse_tokens(tokens, arena)

    if (root_index <= 0) then
        error stop "Failed to parse source code"
    end if

    ctx = create_semantic_context()
    call analyze_semantics(ctx, arena, root_index)
    print *, "✓ Source code successfully parsed and analyzed"
    print *

    ! Build call graph
    print *, "Step 2: Building Call Graph"
    print *, "---------------------------"
    graph = build_call_graph_from_arena(arena, root_index)
    print *, "✓ Call graph constructed"
    print *

    ! Show all procedures
    print *, "Step 3: Discovered Procedures"
    print *, "-----------------------------"
    all_procs = get_all_procedures_in_graph(graph)
    print *, "Total procedures found:", size(all_procs)
    do i = 1, size(all_procs)
        print *, "  ", trim(all_procs(i))
    end do
    print *

    ! Find unused procedures
    print *, "Step 4: Unused Procedure Detection"
    print *, "----------------------------------"
    unused = get_unused_procedures(graph)
    if (size(unused) > 0) then
        print *, "Found", size(unused), "unused procedure(s):"
        do i = 1, size(unused)
            print *, "  ⚠️  ", trim(unused(i))
        end do
    else
        print *, "✓ All procedures are used"
    end if
    print *

    ! Analyze specific procedure relationships
    print *, "Step 5: Procedure Call Analysis"
    print *, "-------------------------------"

    ! Show who calls compute_result
    callers = get_procedure_callers(graph, "compute_result")
    print *, "Procedures that call 'compute_result':"
    if (size(callers) > 0) then
        do i = 1, size(callers)
            print *, "  → ", trim(callers(i))
        end do
    else
        print *, "  (none)"
    end if
    print *

    ! Show what compute_result calls
    callees = get_procedure_callees(graph, "compute_result")
    print *, "Procedures called by 'compute_result':"
    if (size(callees) > 0) then
        do i = 1, size(callees)
            print *, "  → ", trim(callees(i))
        end do
    else
        print *, "  (none)"
    end if
    print *

    ! Show what main calls
    callees = get_procedure_callees(graph, "main")
    print *, "Procedures called by 'main':"
    if (size(callees) > 0) then
        do i = 1, size(callees)
            print *, "  → ", trim(callees(i))
        end do
    else
        print *, "  (none)"
    end if
    print *

    ! Check if specific procedures are used
    print *, "Step 6: Usage Status Check"
    print *, "-------------------------"
    print *, "Is 'helper_function' used?", &
             merge("Yes", "No ", is_procedure_used(graph, "helper_function"))
    print *, "Is 'unused_cleanup' used?", &
             merge("Yes", "No ", is_procedure_used(graph, "unused_cleanup"))
    print *, "Is 'dead_code' used?", &
             merge("Yes", "No ", is_procedure_used(graph, "dead_code"))
    print *

    ! Show call edges
    print *, "Step 7: All Call Edges"
    print *, "----------------------"
    edges = get_call_edges(graph)
    if (size(edges) > 0) then
        print *, "Total call edges:", size(edges)
        do i = 1, size(edges)
            print *, "  ", trim(edges(i)%caller), " → ", trim(edges(i)%callee)
        end do
    else
        print *, "No call edges found"
    end if
    print *

    ! Summary
    print *, "Step 8: Summary"
    print *, "--------------"
    print *, "Total procedures:", size(all_procs)
    print *, "Total call edges:", size(edges)
    print *, "Unused procedures:", size(unused)
    print *, "Usage efficiency:", &
             real(size(all_procs) - size(unused)) / real(size(all_procs)) * 100.0, "%"
    print *

    print *, "=== Call Graph Analysis Complete ==="

end program demo_call_graph