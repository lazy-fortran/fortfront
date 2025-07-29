program test_frontend_full_explicit_params
    use frontend, only: lex_file, parse_tokens
    use lexer_core, only: token_t, tokenize_core
    use ast_core
    use codegen_core, only: generate_code_from_arena
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Full Frontend: Explicit Parameter Declarations ==='
    print *

    ! Test basic explicit type declaration
    if (.not. test_basic_explicit_type()) all_passed = .false.

    ! Test with intent
    if (.not. test_explicit_type_with_intent()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All full frontend explicit parameter tests passed!'
        stop 0
    else
        print *, 'Some full frontend explicit parameter tests failed!'
        stop 1
    end if

contains

    logical function test_basic_explicit_type()
        character(len=:), allocatable :: source, generated_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        test_basic_explicit_type = .false.
        print *, 'Testing basic explicit type through full frontend...'

        ! Test: function with explicit type parameter
        source = &
            "function add(real(8) :: x, y) result(z)"//new_line('a')// &
            "    real(8) :: z"//new_line('a')// &
            "    z = x + y"//new_line('a')// &
            "end function"

        ! Tokenize
        call tokenize_core(source, tokens)

        if (size(tokens) == 0) then
            print *, '  FAIL: No tokens generated'
            return
        end if

        print *, '  Generated ', size(tokens), ' tokens'

        ! Parse
        error_msg = ""
        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (prog_index > 0 .and. len(error_msg) == 0) then
            print *, '  AST created successfully'

            ! Generate code
            generated_code = generate_code_from_arena(arena, prog_index)

            if (len(generated_code) > 0) then
                print *, '  Code generated successfully:'
                print *, '--- Generated Code ---'
                print '(a)', generated_code
                print *, '--- End Generated Code ---'
                test_basic_explicit_type = .true.
            else
                print *, '  FAIL: No code generated'
            end if
        else
            print *, '  FAIL: Parsing failed: ', trim(error_msg)
        end if

    end function test_basic_explicit_type

    logical function test_explicit_type_with_intent()
        character(len=:), allocatable :: source, generated_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        test_explicit_type_with_intent = .false.
        print *, 'Testing explicit type with intent through full frontend...'

        ! Test: subroutine with intent(in) parameters
        source = &
            "subroutine process(real(8), intent(in) :: a, b)"//new_line('a')// &
            "    print *, a + b"//new_line('a')// &
            "end subroutine"

        ! Tokenize
        call tokenize_core(source, tokens)

        if (size(tokens) == 0) then
            print *, '  FAIL: No tokens generated'
            return
        end if

        print *, '  Generated ', size(tokens), ' tokens'

        ! Parse
        error_msg = ""
        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (prog_index > 0 .and. len(error_msg) == 0) then
            print *, '  AST created successfully'

            ! Generate code
            generated_code = generate_code_from_arena(arena, prog_index)

            if (len(generated_code) > 0) then
                print *, '  Code generated successfully:'
                print *, '--- Generated Code ---'
                print '(a)', generated_code
                print *, '--- End Generated Code ---'
                test_explicit_type_with_intent = .true.
            else
                print *, '  FAIL: No code generated'
            end if
        else
            print *, '  FAIL: Parsing failed: ', trim(error_msg)
        end if

    end function test_explicit_type_with_intent

end program test_frontend_full_explicit_params
