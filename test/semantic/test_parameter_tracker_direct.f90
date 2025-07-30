program test_parameter_tracker_direct
    use parameter_tracker
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parameter Tracker Direct Tests ==="
    
    ! Test basic operations
    call test_basic_operations()
    
    ! Test array resizing
    call test_array_resizing()
    
    ! Test intent tracking
    call test_intent_tracking()
    
    ! Test optional parameters
    call test_optional_parameters()
    
    ! Test edge cases
    call test_edge_cases()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parameter tracker tests passed!"
        stop 0
    else
        print *, "Some parameter tracker tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_basic_operations()
        type(parameter_tracker_t) :: tracker
        
        call test_start("Basic parameter addition")
        
        ! Add parameters
        call tracker%add_parameter("x")
        call tracker%add_parameter("y")
        call tracker%add_parameter("z")
        
        if (tracker%count == 3) then
            if (tracker%is_parameter("x") .and. &
                tracker%is_parameter("y") .and. &
                tracker%is_parameter("z")) then
                call test_pass()
            else
                call test_fail("Parameters not found")
            end if
        else
            call test_fail("Wrong parameter count")
        end if
        
        call test_start("Non-existent parameter check")
        
        if (.not. tracker%is_parameter("nonexistent")) then
            call test_pass()
        else
            call test_fail("Should not find non-existent parameter")
        end if
        
        call test_start("Clear operation")
        
        call tracker%clear()
        
        if (tracker%count == 0 .and. .not. tracker%is_parameter("x")) then
            call test_pass()
        else
            call test_fail("Clear operation failed")
        end if
    end subroutine test_basic_operations
    
    subroutine test_array_resizing()
        type(parameter_tracker_t) :: tracker
        integer :: i
        character(len=10) :: param_name
        
        call test_start("Array resizing with many parameters")
        
        ! Add more than initial capacity (10)
        do i = 1, 25
            write(param_name, '(A,I0)') "param", i
            call tracker%add_parameter(trim(param_name))
        end do
        
        if (tracker%count == 25) then
            ! Check first, middle, and last
            if (tracker%is_parameter("param1") .and. &
                tracker%is_parameter("param15") .and. &
                tracker%is_parameter("param25")) then
                call test_pass()
            else
                call test_fail("Parameters lost during resize")
            end if
        else
            call test_fail("Wrong count after resize")
        end if
        
        call tracker%clear()
    end subroutine test_array_resizing
    
    subroutine test_intent_tracking()
        type(parameter_tracker_t) :: tracker
        character(len=:), allocatable :: intent_value
        
        call test_start("Intent attribute tracking")
        
        ! Add parameters with different intents
        call tracker%add_parameter("input_var", intent="in")
        call tracker%add_parameter("output_var", intent="out")
        call tracker%add_parameter("inout_var", intent="inout")
        call tracker%add_parameter("no_intent_var")
        
        ! Check intents
        intent_value = tracker%get_parameter_intent("input_var")
        if (intent_value == "in") then
            intent_value = tracker%get_parameter_intent("output_var")
            if (intent_value == "out") then
                intent_value = tracker%get_parameter_intent("inout_var")
                if (intent_value == "inout") then
                    intent_value = tracker%get_parameter_intent("no_intent_var")
                    if (intent_value == "") then
                        call test_pass()
                    else
                        call test_fail("Wrong intent for no_intent_var")
                    end if
                else
                    call test_fail("Wrong intent for inout_var")
                end if
            else
                call test_fail("Wrong intent for output_var")
            end if
        else
            call test_fail("Wrong intent for input_var")
        end if
        
        call test_start("Intent for non-existent parameter")
        
        intent_value = tracker%get_parameter_intent("nonexistent")
        if (intent_value == "") then
            call test_pass()
        else
            call test_fail("Should return empty for non-existent")
        end if
        
        call tracker%clear()
    end subroutine test_intent_tracking
    
    subroutine test_optional_parameters()
        type(parameter_tracker_t) :: tracker
        
        call test_start("Optional parameter tracking")
        
        ! Add optional and non-optional parameters
        call tracker%add_parameter("required1")
        call tracker%add_parameter("optional1", is_optional=.true.)
        call tracker%add_parameter("required2", intent="in", is_optional=.false.)
        call tracker%add_parameter("optional2", intent="out", is_optional=.true.)
        
        if (tracker%count == 4) then
            ! Check optional flags
            if (.not. tracker%params(1)%is_optional .and. &
                tracker%params(2)%is_optional .and. &
                .not. tracker%params(3)%is_optional .and. &
                tracker%params(4)%is_optional) then
                call test_pass()
            else
                call test_fail("Optional flags incorrect")
            end if
        else
            call test_fail("Wrong parameter count")
        end if
        
        call test_start("Combined intent and optional")
        
        if (tracker%params(4)%intent == "out" .and. &
            tracker%params(4)%is_optional) then
            call test_pass()
        else
            call test_fail("Combined attributes incorrect")
        end if
        
        call tracker%clear()
    end subroutine test_optional_parameters
    
    subroutine test_edge_cases()
        type(parameter_tracker_t) :: tracker
        character(len=:), allocatable :: intent_value
        
        call test_start("Empty tracker operations")
        
        ! Operations on empty tracker
        if (.not. tracker%is_parameter("anything")) then
            intent_value = tracker%get_parameter_intent("anything")
            if (intent_value == "") then
                call test_pass()
            else
                call test_fail("Should return empty intent")
            end if
        else
            call test_fail("Should not find in empty tracker")
        end if
        
        call test_start("Duplicate parameter names")
        
        ! Add same name multiple times
        call tracker%add_parameter("dup", intent="in")
        call tracker%add_parameter("dup", intent="out")
        
        ! Should find first occurrence
        intent_value = tracker%get_parameter_intent("dup")
        if (intent_value == "in") then
            call test_pass()
        else
            call test_fail("Should return first occurrence")
        end if
        
        call test_start("Long parameter names")
        
        call tracker%clear()
        
        ! Test with very long names
        call tracker%add_parameter("this_is_a_very_long_parameter_name_that_might_cause_issues", &
                                  intent="inout", is_optional=.true.)
        
        if (tracker%is_parameter("this_is_a_very_long_parameter_name_that_might_cause_issues")) then
            intent_value = tracker%get_parameter_intent( &
                "this_is_a_very_long_parameter_name_that_might_cause_issues")
            if (intent_value == "inout") then
                call test_pass()
            else
                call test_fail("Long name intent incorrect")
            end if
        else
            call test_fail("Long name not found")
        end if
        
        call tracker%clear()
    end subroutine test_edge_cases
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_parameter_tracker_direct