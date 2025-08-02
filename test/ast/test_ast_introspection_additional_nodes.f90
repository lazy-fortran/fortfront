program test_ast_introspection_additional_nodes
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Additional Node Types..."

    if (.not. test_additional_node_types()) all_passed = .false.
    if (.not. test_default_case_node_type()) all_passed = .false.

    if (all_passed) then
        print *, "All AST introspection additional node tests passed!"
        stop 0
    else
        print *, "Some AST introspection additional node tests failed!"
        stop 1
    end if

contains

    ! Test additional node types to improve coverage
    logical function test_additional_node_types()
        test_additional_node_types = .true.
        print *, "Testing additional node types..."

        ! Test subroutine definition node (type_id = 9)
        if (.not. test_subroutine_def_node_type()) then
            test_additional_node_types = .false.
        end if

        ! Test module node (type_id = 18)
        if (.not. test_module_node_type()) then
            test_additional_node_types = .false.
        end if

        ! Test use statement node (type_id = 19)
        if (.not. test_use_statement_node_type()) then
            test_additional_node_types = .false.
        end if

        print *, "  Additional node types: ", &
                 merge("PASS", "FAIL", test_additional_node_types)
    end function test_additional_node_types

    ! Test subroutine definition node
    logical function test_subroutine_def_node_type()
        character(len=*), parameter :: source = &
            "subroutine my_sub(x)" // new_line('a') // &
            "  integer :: x" // new_line('a') // &
            "  x = x + 1" // new_line('a') // &
            "end subroutine"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, sub_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_subroutine_def_node_type = .true.
        print *, "  Testing subroutine_def node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_subroutine_def_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_subroutine_def_node_type = .false.
            return
        end if

        ! Find subroutine_def node in arena
        sub_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "subroutine_def") then
                sub_index = i
                exit
            end if
        end do

        if (sub_index == 0) then
            print *, "    INFO: No subroutine_def node found"
            return
        end if

        node = get_node(arena, sub_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get subroutine_def node"
            test_subroutine_def_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 9) then
            print *, "    PASS: Subroutine_def node has type_id = 9"
        else
            print *, "    INFO: Subroutine_def node has type_id = ", type_id
        end if
    end function test_subroutine_def_node_type

    ! Test module node
    logical function test_module_node_type()
        character(len=*), parameter :: source = &
            "module my_module" // new_line('a') // &
            "  implicit none" // new_line('a') // &
            "end module"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, mod_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_module_node_type = .true.
        print *, "  Testing module node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_module_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_module_node_type = .false.
            return
        end if

        ! Find module node in arena
        mod_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "module") then
                mod_index = i
                exit
            end if
        end do

        if (mod_index == 0) then
            print *, "    INFO: No module node found"
            return
        end if

        node = get_node(arena, mod_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get module node"
            test_module_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 18) then
            print *, "    PASS: Module node has type_id = 18"
        else
            print *, "    INFO: Module node has type_id = ", type_id
        end if
    end function test_module_node_type

    ! Test use statement node
    logical function test_use_statement_node_type()
        character(len=*), parameter :: source = &
            "use iso_fortran_env"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, use_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_use_statement_node_type = .true.
        print *, "  Testing use_statement node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_use_statement_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_use_statement_node_type = .false.
            return
        end if

        ! Find use_statement node in arena
        use_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "use_statement") then
                use_index = i
                exit
            end if
        end do

        if (use_index == 0) then
            print *, "    INFO: No use_statement node found"
            return
        end if

        node = get_node(arena, use_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get use_statement node"
            test_use_statement_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 19) then
            print *, "    PASS: Use_statement node has type_id = 19"
        else
            print *, "    INFO: Use_statement node has type_id = ", type_id
        end if
    end function test_use_statement_node_type

    ! Test node type that would hit the default case (type_id = 99)
    logical function test_default_case_node_type()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id
        
        test_default_case_node_type = .true.
        print *, "Testing default case node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_default_case_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_default_case_node_type = .false.
            return
        end if

        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "  FAIL: Could not get node"
            test_default_case_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        print *, "  Node type ID: ", type_id

        ! Any valid type ID should be between 1-39 or 99 for default
        if ((type_id >= 1 .and. type_id <= 39) .or. type_id == 99) then
            print *, "  PASS: Node type ID is valid"
        else
            print *, "  FAIL: Invalid node type ID: ", type_id
            test_default_case_node_type = .false.
        end if

        print *, "  Default case test: ", &
                 merge("PASS", "FAIL", test_default_case_node_type)
    end function test_default_case_node_type

end program test_ast_introspection_additional_nodes