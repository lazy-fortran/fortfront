program test_multi_attributes
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, "Running parser tests for multiple attributes..."
    
    if (.not. test_allocatable_pointer()) all_passed = .false.
    if (.not. test_allocatable_target()) all_passed = .false.
    if (.not. test_pointer_target()) all_passed = .false.
    if (.not. test_all_three_attributes()) all_passed = .false.

    if (all_passed) then
        print *, "All multi-attribute tests passed"
        stop 0
    else
        print *, "Some multi-attribute tests failed"
        stop 1
    end if

contains

    logical function test_allocatable_pointer()
        character(len=*), parameter :: source = "real, allocatable, pointer :: x"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        character(len=:), allocatable :: error_msg
        logical :: found_decl, has_allocatable, has_pointer
        
        test_allocatable_pointer = .true.
        print *, "  Testing allocatable and pointer attributes..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_pointer = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_pointer = .false.
                return
            end if
        end if
        
        ! Check for both attributes
        found_decl = .false.
        has_allocatable = .false.
        has_pointer = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "x") then
                        found_decl = .true.
                        has_allocatable = node%is_allocatable
                        has_pointer = node%is_pointer
                    end if
                end select
            end if
        end do
        
        if (.not. found_decl) then
            print *, "    FAIL: Declaration not found"
            test_allocatable_pointer = .false.
        else if (.not. has_allocatable) then
            print *, "    FAIL: Allocatable attribute not set"
            test_allocatable_pointer = .false.
        else if (.not. has_pointer) then
            print *, "    FAIL: Pointer attribute not set"
            test_allocatable_pointer = .false.
        else
            print *, "    PASS: Both allocatable and pointer attributes parsed correctly"
        end if
        
    end function test_allocatable_pointer

    logical function test_allocatable_target()
        character(len=*), parameter :: source = "real, allocatable, target :: arr(:)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        character(len=:), allocatable :: error_msg
        logical :: found_decl, has_allocatable, has_target
        
        test_allocatable_target = .true.
        print *, "  Testing allocatable and target attributes..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_target = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_target = .false.
                return
            end if
        end if
        
        ! Check for both attributes
        found_decl = .false.
        has_allocatable = .false.
        has_target = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "arr") then
                        found_decl = .true.
                        has_allocatable = node%is_allocatable
                        has_target = node%is_target
                    end if
                end select
            end if
        end do
        
        if (.not. found_decl) then
            print *, "    FAIL: Declaration not found"
            test_allocatable_target = .false.
        else if (.not. has_allocatable) then
            print *, "    FAIL: Allocatable attribute not set"
            test_allocatable_target = .false.
        else if (.not. has_target) then
            print *, "    FAIL: Target attribute not set"
            test_allocatable_target = .false.
        else
            print *, "    PASS: Both allocatable and target attributes parsed correctly"
        end if
        
    end function test_allocatable_target

    logical function test_pointer_target()
        character(len=*), parameter :: source = "real, pointer, target :: ptr"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        character(len=:), allocatable :: error_msg
        logical :: found_decl, has_pointer, has_target
        
        test_pointer_target = .true.
        print *, "  Testing pointer and target attributes..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_pointer_target = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_pointer_target = .false.
                return
            end if
        end if
        
        ! Check for both attributes
        found_decl = .false.
        has_pointer = .false.
        has_target = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "ptr") then
                        found_decl = .true.
                        has_pointer = node%is_pointer
                        has_target = node%is_target
                    end if
                end select
            end if
        end do
        
        if (.not. found_decl) then
            print *, "    FAIL: Declaration not found"
            test_pointer_target = .false.
        else if (.not. has_pointer) then
            print *, "    FAIL: Pointer attribute not set"
            test_pointer_target = .false.
        else if (.not. has_target) then
            print *, "    FAIL: Target attribute not set"
            test_pointer_target = .false.
        else
            print *, "    PASS: Both pointer and target attributes parsed correctly"
        end if
        
    end function test_pointer_target

    logical function test_all_three_attributes()
        ! Note: This is technically invalid Fortran (can't have allocatable and pointer)
        ! but we're testing the parser's ability to handle multiple attributes
        character(len=*), parameter :: source = "real, allocatable, pointer, target :: x"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        character(len=:), allocatable :: error_msg
        logical :: found_decl, has_allocatable, has_pointer, has_target
        
        test_all_three_attributes = .true.
        print *, "  Testing all three attributes..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_all_three_attributes = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_all_three_attributes = .false.
                return
            end if
        end if
        
        ! Check for all attributes
        found_decl = .false.
        has_allocatable = .false.
        has_pointer = .false.
        has_target = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "x") then
                        found_decl = .true.
                        has_allocatable = node%is_allocatable
                        has_pointer = node%is_pointer
                        has_target = node%is_target
                    end if
                end select
            end if
        end do
        
        if (.not. found_decl) then
            print *, "    FAIL: Declaration not found"
            test_all_three_attributes = .false.
        else if (.not. has_allocatable) then
            print *, "    FAIL: Allocatable attribute not set"
            test_all_three_attributes = .false.
        else if (.not. has_pointer) then
            print *, "    FAIL: Pointer attribute not set"
            test_all_three_attributes = .false.
        else if (.not. has_target) then
            print *, "    FAIL: Target attribute not set"
            test_all_three_attributes = .false.
        else
            print *, "    PASS: All three attributes parsed correctly"
        end if
        
    end function test_all_three_attributes

end program test_multi_attributes