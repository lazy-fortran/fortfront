program test_source_reconstruction_core
    use source_reconstruction_analyzer, only: source_location_t, &
                                              source_context_t
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== Source Reconstruction Core Infrastructure Tests ==="
    print *, ""

    ! Test 1: Create default source_location_t
    call test_start("Create default source_location_t")
    block
        type(source_location_t) :: loc
        if (loc%line == 0 .and. loc%column == 0 .and. &
            loc%start_char == 0 .and. loc%end_char == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: all fields = 0"
            print *, "  Got: line=", loc%line, " column=", loc%column, &
                     " start_char=", loc%start_char, " end_char=", loc%end_char
        end if
    end block

    ! Test 2: Create explicit source_location_t
    call test_start("Create explicit source_location_t")
    block
        type(source_location_t) :: loc
        loc = source_location_t(line=5, column=10, start_char=50, end_char=75)
        if (loc%line == 5 .and. loc%column == 10 .and. &
            loc%start_char == 50 .and. loc%end_char == 75) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: line=5, column=10, start_char=50, end_char=75"
            print *, "  Got: line=", loc%line, " column=", loc%column, &
                     " start_char=", loc%start_char, " end_char=", loc%end_char
        end if
    end block

    ! Test 3: Create default source_context_t
    call test_start("Create default source_context_t")
    block
        type(source_context_t) :: ctx
        if (.not. allocated(ctx%original_source) .and. &
            .not. allocated(ctx%line_starts) .and. &
            ctx%total_lines == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: unallocated arrays, total_lines=0"
            print *, "  Got: source allocated=", allocated(ctx%original_source), &
                     " line_starts allocated=", allocated(ctx%line_starts), &
                     " total_lines=", ctx%total_lines
        end if
    end block

    ! Test 4: Initialize source_context_t with source text
    call test_start("Initialize source_context_t with source text")
    block
        type(source_context_t) :: ctx
        character(*), parameter :: test_source = "line1" // new_line('a') // &
                                                "line2" // new_line('a') // &
                                                "line3"
        call ctx%initialize_source(test_source)
        
        if (allocated(ctx%original_source) .and. &
            allocated(ctx%line_starts) .and. &
            ctx%total_lines == 3 .and. &
            ctx%original_source == test_source) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: allocated arrays, total_lines=3"
            print *, "  Got: source allocated=", allocated(ctx%original_source), &
                     " line_starts allocated=", allocated(ctx%line_starts), &
                     " total_lines=", ctx%total_lines
            if (allocated(ctx%original_source)) then
                print *, "  Source length=", len(ctx%original_source), &
                         " expected=", len(test_source)
            end if
        end if
    end block

    ! Test 5: Extract line text from source_context_t
    call test_start("Extract line text from source_context_t")
    block
        type(source_context_t) :: ctx
        character(*), parameter :: test_source = "first" // new_line('a') // &
                                                "second" // new_line('a') // &
                                                "third"
        character(:), allocatable :: line_text
        
        call ctx%initialize_source(test_source)
        line_text = ctx%get_line_text(2)
        
        if (line_text == "second") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'second'"
            print *, "  Got: '", line_text, "'"
        end if
    end block

    ! Test 6: Extract source range from context
    call test_start("Extract source range from context")
    block
        type(source_context_t) :: ctx
        character(*), parameter :: test_source = "hello world example"
        character(:), allocatable :: extracted
        type(source_location_t) :: loc
        
        call ctx%initialize_source(test_source)
        loc = source_location_t(line=1, column=7, start_char=7, end_char=11)
        extracted = ctx%extract_range(loc)
        
        if (extracted == "world") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'world'"
            print *, "  Got: '", extracted, "'"
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All source reconstruction core tests passed!"
        stop 0
    else
        print *, "Some source reconstruction core tests failed!"
        stop 1
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

end program test_source_reconstruction_core