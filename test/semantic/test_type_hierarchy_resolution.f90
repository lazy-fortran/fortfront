program test_type_hierarchy_resolution
    ! Verifies that semantic analysis populates the derived-type inheritance
    ! hierarchy and that is_subtype_of resolves transitive (grandchild ->
    ! grandparent) relationships, plus the unknown-parent diagnostic (issue
    ! #2819).
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
        analyze_program
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_transitive_subtype()) all_passed = .false.
    if (.not. test_direct_subtype()) all_passed = .false.
    if (.not. test_non_subtype()) all_passed = .false.
    if (.not. test_unknown_parent_diagnostic()) all_passed = .false.

    if (all_passed) then
        print *, "All type hierarchy resolution tests passed"
        stop 0
    else
        print *, "Some type hierarchy resolution tests failed"
        stop 1
    end if

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, iostat, filesize

        open (newunit=unit, file=filepath, access='stream', form='unformatted', &
            status='old', iostat=iostat)
        if (iostat /= 0) then
            content = ''
            return
        end if
        inquire (unit=unit, size=filesize)
        allocate (character(len=filesize) :: content)
        read (unit, iostat=iostat) content
        close (unit)
        if (iostat /= 0) content = ''
    end subroutine read_example

    subroutine analyze_source(source, ctx, ok)
        character(len=*), intent(in) :: source
        type(semantic_context_t), intent(out) :: ctx
        logical, intent(out) :: ok
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg

        ok = .false.
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) return
        end if

        call create_semantic_context(ctx)
        call analyze_program(ctx, arena, prog_index)
        ok = .true.
    end subroutine analyze_source

    logical function test_transitive_subtype() result(passed)
        character(len=:), allocatable :: source
        type(semantic_context_t) :: ctx
        logical :: ok

        passed = .true.
        call read_example('examples/f90/issue_2819_type_hierarchy_chain.f90', &
            source)
        if (len_trim(source) == 0) then
            write (error_unit, '(A)') 'FAIL: could not read hierarchy example'
            passed = .false.
            return
        end if

        call analyze_source(source, ctx, ok)
        if (.not. ok) then
            write (error_unit, '(A)') 'FAIL: analysis of hierarchy example failed'
            passed = .false.
            return
        end if

        if (.not. ctx%type_hierarchy%is_subtype_of('square_t', 'shape_t')) then
            write (error_unit, '(A)') &
                'FAIL: square_t not resolved as subtype of grandparent shape_t'
            passed = .false.
        end if
    end function test_transitive_subtype

    logical function test_direct_subtype() result(passed)
        character(len=:), allocatable :: source
        type(semantic_context_t) :: ctx
        logical :: ok

        passed = .true.
        call read_example('examples/f90/issue_2819_type_hierarchy_chain.f90', &
            source)
        call analyze_source(source, ctx, ok)
        if (.not. ok) then
            passed = .false.
            return
        end if

        if (.not. ctx%type_hierarchy%is_subtype_of('polygon_t', 'shape_t')) then
            write (error_unit, '(A)') &
                'FAIL: polygon_t not resolved as subtype of shape_t'
            passed = .false.
        end if
    end function test_direct_subtype

    logical function test_non_subtype() result(passed)
        character(len=:), allocatable :: source
        type(semantic_context_t) :: ctx
        logical :: ok

        passed = .true.
        call read_example('examples/f90/issue_2819_type_hierarchy_chain.f90', &
            source)
        call analyze_source(source, ctx, ok)
        if (.not. ok) then
            passed = .false.
            return
        end if

        if (ctx%type_hierarchy%is_subtype_of('shape_t', 'square_t')) then
            write (error_unit, '(A)') &
                'FAIL: shape_t wrongly reported as subtype of square_t'
            passed = .false.
        end if
    end function test_non_subtype

    logical function test_unknown_parent_diagnostic() result(passed)
        character(len=:), allocatable :: source
        type(semantic_context_t) :: ctx
        logical :: ok

        passed = .true.
        ! Focused negative unit input: extends a type that is never defined.
        source = 'program p'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            '    type, extends(missing_t) :: child_t'//new_line('a')// &
            '        integer :: v'//new_line('a')// &
            '    end type child_t'//new_line('a')// &
            '    type(child_t) :: c'//new_line('a')// &
            '    c%v = 1'//new_line('a')// &
            'end program p'

        call analyze_source(source, ctx, ok)
        if (.not. ok) then
            write (error_unit, '(A)') 'FAIL: analysis of unknown-parent failed'
            passed = .false.
            return
        end if

        if (.not. ctx%errors%has_errors()) then
            write (error_unit, '(A)') &
                'FAIL: extends of undefined parent produced no diagnostic'
            passed = .false.
        end if
    end function test_unknown_parent_diagnostic

end program test_type_hierarchy_resolution
