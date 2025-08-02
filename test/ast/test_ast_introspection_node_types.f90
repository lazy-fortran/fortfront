program test_ast_introspection_node_types
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Node Type Coverage..."

    if (.not. test_get_node_type_id_coverage()) all_passed = .false.

    if (all_passed) then
        print *, "All AST introspection node type tests passed!"
        stop 0
    else
        print *, "Some AST introspection node type tests failed!"
        stop 1
    end if

contains

    ! Test get_node_type_id with different node types
    logical function test_get_node_type_id_coverage()
        test_get_node_type_id_coverage = .true.
        print *, "Testing get_node_type_id with different node types..."

        ! Test program node (type_id = 1)
        if (.not. test_program_node_type()) then
            test_get_node_type_id_coverage = .false.
        end if

        ! Test assignment node (type_id = 3)  
        if (.not. test_assignment_node_type()) then
            test_get_node_type_id_coverage = .false.
        end if

        ! Test binary_op node (type_id = 4)
        if (.not. test_binary_op_node_type()) then
            test_get_node_type_id_coverage = .false.
        end if

        ! Test identifier node (type_id = 5)
        if (.not. test_identifier_node_type()) then
            test_get_node_type_id_coverage = .false.
        end if

        ! Test literal node (type_id = 6)
        if (.not. test_literal_node_type()) then
            test_get_node_type_id_coverage = .false.
        end if

        ! Test function definition node (type_id = 2)
        if (.not. test_function_def_node_type()) then
            test_get_node_type_id_coverage = .false.
        end if

        print *, "  get_node_type_id coverage: ", &
                 merge("PASS", "FAIL", test_get_node_type_id_coverage)
    end function test_get_node_type_id_coverage

    ! Test program node type
    logical function test_program_node_type()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id
        
        test_program_node_type = .true.
        print *, "  Testing program node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_program_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_program_node_type = .false.
            return
        end if

        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get root node"
            test_program_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 1) then
            print *, "    PASS: Program node has type_id = 1"
        else
            print *, "    FAIL: Expected type_id=1, got ", type_id
            test_program_node_type = .false.
        end if
    end function test_program_node_type

    ! Test assignment node type
    logical function test_assignment_node_type()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, assignment_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_assignment_node_type = .true.
        print *, "  Testing assignment node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_assignment_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_assignment_node_type = .false.
            return
        end if

        ! Find assignment node in arena
        assignment_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "assignment") then
                assignment_index = i
                exit
            end if
        end do

        if (assignment_index == 0) then
            print *, "    FAIL: No assignment node found"
            test_assignment_node_type = .false.
            return
        end if

        node = get_node(arena, assignment_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get assignment node"
            test_assignment_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 3) then
            print *, "    PASS: Assignment node has type_id = 3"
        else
            print *, "    FAIL: Expected type_id=3, got ", type_id
            test_assignment_node_type = .false.
        end if
    end function test_assignment_node_type

    ! Test binary_op node type
    logical function test_binary_op_node_type()
        character(len=*), parameter :: source = "x = 1 + 2"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, binary_op_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_binary_op_node_type = .true.
        print *, "  Testing binary_op node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_binary_op_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_binary_op_node_type = .false.
            return
        end if

        ! Find binary_op node in arena
        binary_op_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "binary_op") then
                binary_op_index = i
                exit
            end if
        end do

        if (binary_op_index == 0) then
            print *, "    FAIL: No binary_op node found"
            test_binary_op_node_type = .false.
            return
        end if

        node = get_node(arena, binary_op_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get binary_op node"
            test_binary_op_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 4) then
            print *, "    PASS: Binary_op node has type_id = 4"
        else
            print *, "    FAIL: Expected type_id=4, got ", type_id
            test_binary_op_node_type = .false.
        end if
    end function test_binary_op_node_type

    ! Test identifier node type
    logical function test_identifier_node_type()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, identifier_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_identifier_node_type = .true.
        print *, "  Testing identifier node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_identifier_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_identifier_node_type = .false.
            return
        end if

        ! Find identifier node in arena
        identifier_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "identifier") then
                identifier_index = i
                exit
            end if
        end do

        if (identifier_index == 0) then
            print *, "    FAIL: No identifier node found"
            test_identifier_node_type = .false.
            return
        end if

        node = get_node(arena, identifier_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get identifier node"
            test_identifier_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 5) then
            print *, "    PASS: Identifier node has type_id = 5"
        else
            print *, "    FAIL: Expected type_id=5, got ", type_id
            test_identifier_node_type = .false.
        end if
    end function test_identifier_node_type

    ! Test literal node type
    logical function test_literal_node_type()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, literal_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_literal_node_type = .true.
        print *, "  Testing literal node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed"
            test_literal_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed"
            test_literal_node_type = .false.
            return
        end if

        ! Find literal node in arena
        literal_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "literal") then
                literal_index = i
                exit
            end if
        end do

        if (literal_index == 0) then
            print *, "    FAIL: No literal node found"
            test_literal_node_type = .false.
            return
        end if

        node = get_node(arena, literal_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get literal node"
            test_literal_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 6) then
            print *, "    PASS: Literal node has type_id = 6"
        else
            print *, "    FAIL: Expected type_id=6, got ", type_id
            test_literal_node_type = .false.
        end if
    end function test_literal_node_type

    ! Test function definition node type
    logical function test_function_def_node_type()
        character(len=*), parameter :: source = &
            "function add(a, b)" // new_line('a') // &
            "  add = a + b" // new_line('a') // &
            "end function"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, function_index
        character(len=:), allocatable :: error_msg  
        class(ast_node), allocatable :: node
        integer :: type_id, i
        
        test_function_def_node_type = .true.
        print *, "  Testing function_def node type..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed: ", error_msg
            test_function_def_node_type = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed: ", error_msg
            test_function_def_node_type = .false.
            return
        end if

        ! Find function_def node in arena
        function_index = 0
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "function_def") then
                function_index = i
                exit
            end if
        end do

        if (function_index == 0) then
            print *, "    FAIL: No function_def node found"
            test_function_def_node_type = .false.
            return
        end if

        node = get_node(arena, function_index)
        if (.not. allocated(node)) then
            print *, "    FAIL: Could not get function_def node"
            test_function_def_node_type = .false.
            return
        end if

        type_id = get_node_type_id(node)
        if (type_id == 2) then
            print *, "    PASS: Function_def node has type_id = 2"
        else
            print *, "    FAIL: Expected type_id=2, got ", type_id
            test_function_def_node_type = .false.
        end if
    end function test_function_def_node_type

end program test_ast_introspection_node_types