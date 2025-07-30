program test_codegen_indent_direct
    use codegen_indent
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Codegen Indent Direct Tests ==="
    
    ! Test indentation functionality
    call test_basic_indent()
    call test_increase_decrease()
    call test_with_indent()
    call test_indent_lines()
    call test_reset_indent()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All codegen indent tests passed!"
        stop 0
    else
        print *, "Some codegen indent tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_basic_indent()
        character(len=:), allocatable :: indent
        
        call test_start("Basic indent retrieval")
        
        call reset_indent()
        indent = get_indent()
        
        if (len(indent) == 0) then
            call test_pass()
        else
            call test_fail("Initial indent should be empty")
        end if
    end subroutine test_basic_indent
    
    subroutine test_increase_decrease()
        character(len=:), allocatable :: indent
        
        call test_start("Increase/decrease indent")
        
        call reset_indent()
        call increase_indent()
        indent = get_indent()
        
        if (len(indent) == 4) then  ! INDENT_SIZE = 4
            call decrease_indent()
            indent = get_indent()
            if (len(indent) == 0) then
                call test_pass()
            else
                call test_fail("Decrease should restore zero indent")
            end if
        else
            call test_fail("Increase should add 4 spaces")
        end if
    end subroutine test_increase_decrease
    
    subroutine test_with_indent()
        character(len=:), allocatable :: indented
        
        call test_start("With indent function")
        
        call reset_indent()
        call increase_indent()
        indented = with_indent("hello")
        
        if (indented == "    hello") then
            call test_pass()
        else
            call test_fail("Expected '    hello' but got '" // indented // "'")
        end if
    end subroutine test_with_indent
    
    subroutine test_indent_lines()
        character(len=:), allocatable :: input, output, expected
        
        call test_start("Indent multiple lines")
        
        call reset_indent()
        call increase_indent()
        
        input = "line1" // new_line('a') // "line2" // new_line('a') // "line3"
        expected = "    line1" // new_line('a') // "    line2" // new_line('a') // "    line3"
        
        output = indent_lines(input)
        
        if (output == expected) then
            call test_pass()
        else
            call test_fail("Multi-line indentation incorrect")
        end if
    end subroutine test_indent_lines
    
    subroutine test_reset_indent()
        character(len=:), allocatable :: indent
        
        call test_start("Reset indent")
        
        call increase_indent()
        call increase_indent()
        call reset_indent()
        indent = get_indent()
        
        if (len(indent) == 0) then
            call test_pass()
        else
            call test_fail("Reset should clear all indentation")
        end if
    end subroutine test_reset_indent
    
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
    
end program test_codegen_indent_direct