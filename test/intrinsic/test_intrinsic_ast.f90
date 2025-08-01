program test_intrinsic_ast
    use ast_core
    use ast_factory
    implicit none

    type(ast_arena_t) :: arena
    integer :: call_index, test_count, passed_count
    type(call_or_subscript_node) :: call_node
    integer, allocatable :: empty_args(:)

    test_count = 0
    passed_count = 0

    print *, "Testing intrinsic function AST integration..."

    ! Initialize arena
    arena = create_ast_arena()

    ! Test intrinsic function call creation via factory
    allocate(empty_args(0))
    call_index = push_call_or_subscript(arena, "abs", empty_args, 1, 1)
    
    test_count = test_count + 1
    if (allocated(arena%entries(call_index)%node)) then
        select type(node => arena%entries(call_index)%node)
        type is (call_or_subscript_node)
            call_node = node
            if (call_node%is_intrinsic .and. &
                allocated(call_node%intrinsic_signature) .and. &
                call_node%intrinsic_signature == "numeric(numeric)") then
                passed_count = passed_count + 1
                print *, "PASS: AST node for 'abs' correctly identified as intrinsic"
            else
                print *, "FAIL: AST node for 'abs' not correctly identified"
                print *, "      is_intrinsic=", call_node%is_intrinsic
                if (allocated(call_node%intrinsic_signature)) then
                    print *, "      signature=", call_node%intrinsic_signature
                else
                    print *, "      signature=<not allocated>"
                end if
            end if
        class default
            print *, "FAIL: Wrong node type returned for 'abs'"
        end select
    else
        print *, "FAIL: Node not allocated for 'abs'"
    end if

    ! Test non-intrinsic function
    call_index = push_call_or_subscript(arena, "user_function", empty_args, 1, 1)
    
    test_count = test_count + 1
    if (allocated(arena%entries(call_index)%node)) then
        select type(node => arena%entries(call_index)%node)
        type is (call_or_subscript_node)
            call_node = node
            if (.not. call_node%is_intrinsic .and. &
                (.not. allocated(call_node%intrinsic_signature) .or. &
                 call_node%intrinsic_signature == "")) then
                passed_count = passed_count + 1
                print *, "PASS: AST node for 'user_function' correctly identified as non-intrinsic"
            else
                print *, "FAIL: AST node for 'user_function' incorrectly identified as intrinsic"
                print *, "      is_intrinsic=", call_node%is_intrinsic
                if (allocated(call_node%intrinsic_signature)) then
                    print *, "      signature=", call_node%intrinsic_signature
                else
                    print *, "      signature=<not allocated>"
                end if
            end if
        class default
            print *, "FAIL: Wrong node type returned for 'user_function'"
        end select
    else
        print *, "FAIL: Node not allocated for 'user_function'"
    end if

    ! Test case insensitive intrinsic identification
    call_index = push_call_or_subscript(arena, "SIN", empty_args, 1, 1)
    
    test_count = test_count + 1
    if (allocated(arena%entries(call_index)%node)) then
        select type(node => arena%entries(call_index)%node)
        type is (call_or_subscript_node)
            call_node = node
            if (call_node%is_intrinsic .and. &
                allocated(call_node%intrinsic_signature) .and. &
                call_node%intrinsic_signature == "real(numeric)") then
                passed_count = passed_count + 1
                print *, "PASS: AST node for 'SIN' correctly identified as intrinsic (case insensitive)"
            else
                print *, "FAIL: AST node for 'SIN' not correctly identified (case insensitive)"
                print *, "      is_intrinsic=", call_node%is_intrinsic
                if (allocated(call_node%intrinsic_signature)) then
                    print *, "      signature=", call_node%intrinsic_signature
                else
                    print *, "      signature=<not allocated>"
                end if
            end if
        class default
            print *, "FAIL: Wrong node type returned for 'SIN'"
        end select
    else
        print *, "FAIL: Node not allocated for 'SIN'"
    end if

    ! Test direct create_call_or_subscript function
    call_node = create_call_or_subscript("sqrt", empty_args, 1, 1)
    
    test_count = test_count + 1
    if (call_node%is_intrinsic .and. &
        allocated(call_node%intrinsic_signature) .and. &
        call_node%intrinsic_signature == "real(numeric)") then
        passed_count = passed_count + 1
        print *, "PASS: Direct create_call_or_subscript for 'sqrt' works correctly"
    else
        print *, "FAIL: Direct create_call_or_subscript for 'sqrt' failed"
        print *, "      is_intrinsic=", call_node%is_intrinsic
        if (allocated(call_node%intrinsic_signature)) then
            print *, "      signature=", call_node%intrinsic_signature
        else
            print *, "      signature=<not allocated>"
        end if
    end if

    ! Test multiple intrinsic functions
    call test_multiple_intrinsics(arena, test_count, passed_count)

    ! Summary
    print *, ""
    print *, "Test Results:"
    print *, "  Total tests: ", test_count
    print *, "  Passed:      ", passed_count
    print *, "  Failed:      ", test_count - passed_count
    
    if (passed_count == test_count) then
        print *, "All AST intrinsic tests passed!"
        stop 0
    else
        print *, "Some AST intrinsic tests failed!"
        stop 1
    end if

contains

    subroutine test_multiple_intrinsics(arena, test_count, passed_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: test_count, passed_count
        character(len=16), parameter :: intrinsic_names(5) = &
            ["int   ", "real  ", "size  ", "trim  ", "len   "]
        character(len=32), parameter :: expected_sigs(5) = &
            ["integer(numeric)           ", &
             "real(numeric)              ", &
             "integer(array,integer?)    ", &
             "character(character)       ", &
             "integer(character)         "]
        integer :: i, call_index
        type(call_or_subscript_node) :: call_node
        integer, allocatable :: empty_args(:)

        allocate(empty_args(0))

        do i = 1, size(intrinsic_names)
            call_index = push_call_or_subscript(arena, trim(intrinsic_names(i)), empty_args, i, 1)
            test_count = test_count + 1
            
            if (allocated(arena%entries(call_index)%node)) then
                select type(node => arena%entries(call_index)%node)
                type is (call_or_subscript_node)
                    call_node = node
                    if (call_node%is_intrinsic .and. &
                        allocated(call_node%intrinsic_signature) .and. &
                        trim(call_node%intrinsic_signature) == trim(expected_sigs(i))) then
                        passed_count = passed_count + 1
                        print *, "PASS: Multiple intrinsic test for '", trim(intrinsic_names(i)), "'"
                    else
                        print *, "FAIL: Multiple intrinsic test for '", trim(intrinsic_names(i)), "'"
                        print *, "      Expected: ", trim(expected_sigs(i))
                        if (allocated(call_node%intrinsic_signature)) then
                            print *, "      Got:      ", trim(call_node%intrinsic_signature)
                        else
                            print *, "      Got:      <not allocated>"
                        end if
                    end if
                class default
                    print *, "FAIL: Wrong node type for '", trim(intrinsic_names(i)), "'"
                end select
            else
                print *, "FAIL: Node not allocated for '", trim(intrinsic_names(i)), "'"
            end if
        end do
    end subroutine test_multiple_intrinsics

end program test_intrinsic_ast