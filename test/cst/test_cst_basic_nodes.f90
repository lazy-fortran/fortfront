program test_cst_basic_nodes
    use cst_nodes
    use cst_arena
    use cst_core
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== CST Basic Node Tests ==="
    print *, ""

    ! Test 1: Create basic CST node
    call test_start("Create basic CST node")
    block
        type(cst_node_t) :: node
        
        node = create_cst_node(CST_IDENTIFIER, 10, 20)
        
        if (node%kind == CST_IDENTIFIER .and. &
            node%start_pos == 10 .and. &
            node%end_pos == 20) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: kind=", CST_IDENTIFIER, ", start=10, end=20"
            print *, "  Got: kind=", node%kind, ", start=", node%start_pos, ", end=", node%end_pos
        end if
    end block

    ! Test 2: Create trivia
    call test_start("Create trivia")
    block
        type(trivia_t) :: trivia
        
        trivia = create_trivia(CST_COMMENT, "! This is a comment", 1, 19)
        
        if (trivia%kind == CST_COMMENT .and. &
            trivia%text == "! This is a comment" .and. &
            trivia%start_pos == 1 .and. &
            trivia%end_pos == 19) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: kind=", CST_COMMENT, ", text='! This is a comment'"
            print *, "  Got: kind=", trivia%kind, ", text='", trivia%text, "'"
        end if
    end block

    ! Test 3: Get node kind names
    call test_start("Get node kind names")
    block
        character(len=:), allocatable :: name
        
        name = get_node_kind_name(CST_PROGRAM)
        if (name /= "PROGRAM") then
            call test_fail()
            print *, "  Expected: 'PROGRAM', Got: '", name, "'"
            return
        end if
        
        name = get_node_kind_name(CST_IDENTIFIER)
        if (name /= "IDENTIFIER") then
            call test_fail()
            print *, "  Expected: 'IDENTIFIER', Got: '", name, "'"
            return
        end if
        
        name = get_node_kind_name(CST_COMMENT)
        if (name /= "COMMENT") then
            call test_fail()
            print *, "  Expected: 'COMMENT', Got: '", name, "'"
            return
        end if
        
        call test_pass()
    end block

    ! Test 4: Check trivia kinds
    call test_start("Check trivia kinds")
    block
        logical :: result
        
        result = is_trivia_kind(CST_COMMENT)
        if (.not. result) then
            call test_fail()
            print *, "  Expected: CST_COMMENT is trivia"
            return
        end if
        
        result = is_trivia_kind(CST_WHITESPACE)
        if (.not. result) then
            call test_fail()
            print *, "  Expected: CST_WHITESPACE is trivia"
            return
        end if
        
        result = is_trivia_kind(CST_IDENTIFIER)
        if (result) then
            call test_fail()
            print *, "  Expected: CST_IDENTIFIER is not trivia"
            return
        end if
        
        call test_pass()
    end block

    ! Test 5: Create and use CST arena
    call test_start("Create and use CST arena")
    block
        type(cst_arena_t) :: arena
        type(cst_node_t) :: node, retrieved_node
        type(cst_handle_t) :: handle
        
        arena = create_cst_arena(10)
        node = create_cst_node(CST_PROGRAM, 1, 100)
        
        handle = arena%push(node)
        
        if (handle%index /= 1) then
            call test_fail()
            print *, "  Expected: handle index = 1, Got:", handle%index
            return
        end if
        
        retrieved_node = arena%get(handle)
        if (retrieved_node%kind < 0) then
            call test_fail()
            print *, "  Expected: valid retrieved node"
            return
        end if
        
        if (retrieved_node%kind /= CST_PROGRAM) then
            call test_fail()
            print *, "  Expected: node kind = CST_PROGRAM, Got:", retrieved_node%kind
            return
        end if
        
        if (retrieved_node%uid <= 0) then
            call test_fail()
            print *, "  Expected: positive UID assigned, Got:", retrieved_node%uid
            return
        end if
        
        call test_pass()
    end block

    ! Test 6: Validate CST nodes
    call test_start("Validate CST nodes")
    block
        type(cst_node_t) :: valid_node, invalid_node
        logical :: result
        
        ! Valid node
        valid_node = create_cst_node(CST_IDENTIFIER, 10, 20)
        result = validate_cst_node(valid_node)
        if (.not. result) then
            call test_fail()
            print *, "  Expected: valid node validates as true"
            return
        end if
        
        ! Invalid node (bad positions)
        invalid_node = create_cst_node(CST_IDENTIFIER, 20, 10)  ! end < start
        result = validate_cst_node(invalid_node)
        if (result) then
            call test_fail()
            print *, "  Expected: invalid node validates as false"
            return
        end if
        
        call test_pass()
    end block

    ! Test 7: Validate trivia
    call test_start("Validate trivia")
    block
        type(trivia_t) :: valid_trivia, invalid_trivia
        logical :: result
        
        ! Valid trivia
        valid_trivia = create_trivia(CST_COMMENT, "! comment", 1, 9)
        result = validate_trivia(valid_trivia)
        if (.not. result) then
            call test_fail()
            print *, "  Expected: valid trivia validates as true"
            return
        end if
        
        ! Invalid trivia (bad kind)
        invalid_trivia = create_trivia(CST_IDENTIFIER, "not trivia", 1, 10)
        result = validate_trivia(invalid_trivia)
        if (result) then
            call test_fail()
            print *, "  Expected: invalid trivia validates as false"
            return
        end if
        
        call test_pass()
    end block

    ! Test 8: Arena handle validation
    call test_start("Arena handle validation")
    block
        type(cst_arena_t) :: arena
        type(cst_node_t) :: node
        type(cst_handle_t) :: valid_handle, invalid_handle
        logical :: result
        
        arena = create_cst_arena()
        node = create_cst_node(CST_LITERAL, 5, 15)
        valid_handle = arena%push(node)
        
        ! Valid handle
        result = arena%is_valid_handle(valid_handle)
        if (.not. result) then
            call test_fail()
            print *, "  Expected: valid handle is valid"
            return
        end if
        
        ! Invalid handle (bad index)
        invalid_handle%index = 999
        invalid_handle%generation = valid_handle%generation
        result = arena%is_valid_handle(invalid_handle)
        if (result) then
            call test_fail()
            print *, "  Expected: invalid handle is invalid"
            return
        end if
        
        call test_pass()
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All CST basic node tests passed!"
    else
        print *, "Some CST basic node tests failed!"
        error stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_cst_basic_nodes