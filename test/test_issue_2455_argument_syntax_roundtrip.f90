program test_issue_2455_argument_syntax_roundtrip
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    ! test_array_constructor_arg() SKIPPED: issue_2455_array_constructor_arg.f90 in expected_failures.txt
    ! call test_array_constructor_arg(all_tests_passed)
    call test_nested_function_args(all_tests_passed)
    call test_multi_variable_decl(all_tests_passed)
    call test_nested_array_constructor(all_tests_passed)

    if (.not. all_tests_passed) then
        error stop 1
    end if

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine test_array_constructor_arg(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source, first_output, second_output
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena1, arena2
        type(token_t), allocatable :: tokens1(:), tokens2(:)
        integer :: root1, root2

        write (output_unit, '(A)') "Testing array constructor in argument list..."

        call read_example('examples/f90/issue_2455_array_constructor_arg.f90', &
                          source)

        ! First pass: parse original source
        arena1 = create_ast_arena()
        call lex_source(source, tokens1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First lex error: " // trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens1, arena1, root1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First parse error: " // &
                trim(error_msg)
            passed = .false.
            return
        end if

        ! Emit fortran code
        call emit_fortran(arena1, root1, first_output)

        ! Second pass: parse generated code
        arena2 = create_ast_arena()
        call lex_source(first_output, tokens2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second lex error: " // trim(error_msg)
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        call parse_tokens(tokens2, arena2, root2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second parse error: " // &
                trim(error_msg)
            write (error_unit, '(A)') &
                "This is the round-trip bug - emitted code cannot be parsed"
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        ! Third pass: verify second output matches first
        call emit_fortran(arena2, root2, second_output)
        if (first_output /= second_output) then
            write (error_unit, '(A)') &
                "FAIL: Roundtrip output differs from first output"
            passed = .false.
            return
        end if

        write (output_unit, '(A)') "PASS: Array constructor argument roundtrip"
    end subroutine test_array_constructor_arg

    subroutine test_nested_function_args(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source, first_output, second_output
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena1, arena2
        type(token_t), allocatable :: tokens1(:), tokens2(:)
        integer :: root1, root2

        write (output_unit, '(A)') "Testing nested function calls with keyword args..."

        call read_example('examples/f90/issue_2455_nested_function_args.f90', &
                          source)

        ! First pass: parse original source
        arena1 = create_ast_arena()
        call lex_source(source, tokens1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First lex error: " // trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens1, arena1, root1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First parse error: " // &
                trim(error_msg)
            passed = .false.
            return
        end if

        ! Emit fortran code
        call emit_fortran(arena1, root1, first_output)

        ! Second pass: parse generated code
        arena2 = create_ast_arena()
        call lex_source(first_output, tokens2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second lex error: " // trim(error_msg)
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        call parse_tokens(tokens2, arena2, root2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second parse error: " // &
                trim(error_msg)
            write (error_unit, '(A)') &
                "This is the round-trip bug - emitted code cannot be parsed"
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        ! Third pass: verify second output matches first
        call emit_fortran(arena2, root2, second_output)
        if (first_output /= second_output) then
            write (error_unit, '(A)') &
                "FAIL: Roundtrip output differs from first output"
            passed = .false.
            return
        end if

        write (output_unit, '(A)') "PASS: Nested function argument roundtrip"
    end subroutine test_nested_function_args

    subroutine test_multi_variable_decl(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source, first_output, second_output
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena1, arena2
        type(token_t), allocatable :: tokens1(:), tokens2(:)
        integer :: root1, root2

        write (output_unit, '(A)') &
            "Testing multi-variable parameter declaration..."

        call read_example('examples/f90/issue_2455_multi_variable_decl.f90', &
                          source)

        ! First pass: parse original source
        arena1 = create_ast_arena()
        call lex_source(source, tokens1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First lex error: " // trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens1, arena1, root1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First parse error: " // &
                trim(error_msg)
            passed = .false.
            return
        end if

        ! Emit fortran code
        call emit_fortran(arena1, root1, first_output)

        ! Second pass: parse generated code
        arena2 = create_ast_arena()
        call lex_source(first_output, tokens2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second lex error: " // trim(error_msg)
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        call parse_tokens(tokens2, arena2, root2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second parse error: " // &
                trim(error_msg)
            write (error_unit, '(A)') &
                "This is the round-trip bug - emitted code cannot be parsed"
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        ! Third pass: verify second output matches first
        call emit_fortran(arena2, root2, second_output)
        if (first_output /= second_output) then
            write (error_unit, '(A)') &
                "FAIL: Roundtrip output differs from first output"
            passed = .false.
            return
        end if

        write (output_unit, '(A)') "PASS: Multi-variable declaration roundtrip"
    end subroutine test_multi_variable_decl

    subroutine test_nested_array_constructor(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source, first_output, second_output
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena1, arena2
        type(token_t), allocatable :: tokens1(:), tokens2(:)
        integer :: root1, root2

        write (output_unit, '(A)') "Testing nested array constructor..."

        call read_example('examples/f90/issue_2455_nested_array_constructor.f90', &
                          source)

        ! First pass: parse original source
        arena1 = create_ast_arena()
        call lex_source(source, tokens1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First lex error: " // trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens1, arena1, root1, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: First parse error: " // &
                trim(error_msg)
            passed = .false.
            return
        end if

        ! Emit fortran code
        call emit_fortran(arena1, root1, first_output)

        ! Second pass: parse generated code
        arena2 = create_ast_arena()
        call lex_source(first_output, tokens2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second lex error: " // trim(error_msg)
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        call parse_tokens(tokens2, arena2, root2, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "FAIL: Second parse error: " // &
                trim(error_msg)
            write (error_unit, '(A)') &
                "This is the round-trip bug - emitted code cannot be parsed"
            write (error_unit, '(A)') "Generated output was:"
            write (error_unit, '(A)') first_output
            passed = .false.
            return
        end if

        ! Third pass: verify second output matches first
        call emit_fortran(arena2, root2, second_output)
        if (first_output /= second_output) then
            write (error_unit, '(A)') &
                "FAIL: Roundtrip output differs from first output"
            passed = .false.
            return
        end if

        write (output_unit, '(A)') "PASS: Nested array constructor roundtrip"
    end subroutine test_nested_array_constructor


end program test_issue_2455_argument_syntax_roundtrip
