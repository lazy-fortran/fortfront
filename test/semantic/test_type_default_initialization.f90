program test_type_default_initialization
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Type Default Field Initialization ==='
    print *

    ! Test parsing and code generation of derived type with default values
    if (.not. test_default_field_parsing()) all_passed = .false.
    if (.not. test_constructor_default_initialization()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All type default initialization tests passed!"
        stop 0
    else
        print *, "Some type default initialization tests failed!"
        stop 1
    end if

contains

    function test_default_field_parsing() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "type :: aesthetic_settings_t" // new_line('a') // &
            "    logical :: optimize_line_breaks = .true." // new_line('a') // &
            "    integer :: max_line_length = 88" // new_line('a') // &
            "end type" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "default_field_parsing"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Tokenization failed: ", error_msg
                return
            end if
        end if

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Parsing failed: ", error_msg
                return
            end if
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, result)
        
        ! Check if compilation succeeded
        if (.not. allocated(result)) then
            print *, "ERROR in ", test_name, ": Compilation failed - no result"
            return
        end if
        
        ! Check that it's properly parsed as a derived type, not individual variables
        if (index(result, "type :: aesthetic_settings_t") == 0) then
            print *, "ERROR in ", test_name, ": Type definition not properly parsed"
            print *, "Got result:"
            print *, result
            return
        end if
        
        ! Check that default values are preserved in output  
        if (index(result, "optimize_line_breaks = .true.") == 0) then
            print *, "ERROR in ", test_name, ": Default logical value not preserved"
            print *, "Got result:"
            print *, result
            return
        end if
        
        if (index(result, "max_line_length = 88") == 0) then
            print *, "ERROR in ", test_name, ": Default integer value not preserved"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_default_field_parsing

    function test_constructor_default_initialization() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "type :: aesthetic_settings_t" // new_line('a') // &
            "    logical :: optimize_line_breaks = .true." // new_line('a') // &
            "    integer :: max_line_length = 88" // new_line('a') // &
            "end type" // new_line('a') // &
            new_line('a') // &
            "function create_aesthetic_settings() result(settings)" // new_line('a') // &
            "    type(aesthetic_settings_t) :: settings" // new_line('a') // &
            "    ! Rely on default initialization" // new_line('a') // &
            "end function" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "constructor_default_initialization"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Tokenization failed: ", error_msg
                return
            end if
        end if

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Parsing failed: ", error_msg
                return
            end if
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, result)
        
        ! Check if compilation succeeded
        if (.not. allocated(result)) then
            print *, "ERROR in ", test_name, ": Compilation failed - no result"
            return
        end if
        
        ! Verify that type definition includes default values
        if (index(result, "optimize_line_breaks = .true.") == 0) then
            print *, "ERROR in ", test_name, ": Type default logical value missing"
            print *, "Got result:"
            print *, result
            return
        end if
        
        if (index(result, "max_line_length = 88") == 0) then
            print *, "ERROR in ", test_name, ": Type default integer value missing"
            print *, "Got result:"
            print *, result
            return
        end if
        
        ! Check that function preserves the semantics (no explicit initialization needed)
        ! The constructor function should not need manual field assignments
        if (index(result, "settings%optimize_line_breaks = .true.") > 0 .or. &
            index(result, "settings%max_line_length = 88") > 0) then
            print *, "ERROR in ", test_name, ": Constructor should rely on defaults, not explicit assignment"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_constructor_default_initialization

end program test_type_default_initialization