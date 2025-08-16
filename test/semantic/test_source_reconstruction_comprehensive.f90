program test_source_reconstruction_comprehensive
    use source_reconstruction_analyzer, only: source_reconstruction_analyzer_t, &
                                              source_context_t, source_location_t
    use ast_core, only: ast_arena_t, create_ast_arena, &
                        create_identifier, identifier_node, &
                        create_literal, literal_node, LITERAL_INTEGER
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== Comprehensive Source Reconstruction Tests ==="
    print *, ""

    ! Test 1: Extract text span with real source text
    call test_start("Extract text span with real source text")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        character(*), parameter :: test_source = &
            "program test" // new_line('a') // &
            "  integer :: x, y" // new_line('a') // &
            "  x = 42" // new_line('a') // &
            "end program"
        character(:), allocatable :: extracted
        
        ! Initialize with real source
        analyzer%result%original_source = test_source
        analyzer%result%total_lines = 4
        analyzer%analysis_complete = .true.
        
        ! Extract "integer" from line 2, columns 3-9
        extracted = analyzer%extract_text_span(2, 3, 2, 9)
        
        if (extracted == "integer") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'integer'"
            print *, "  Got: '", extracted, "'"
        end if
    end block

    ! Test 2: Get line text with real source
    call test_start("Get line text with real source")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        character(*), parameter :: test_source = &
            "program test" // new_line('a') // &
            "  integer :: x, y" // new_line('a') // &
            "  x = 42" // new_line('a') // &
            "end program"
        character(:), allocatable :: line_text
        
        ! Initialize with real source
        analyzer%result%original_source = test_source
        analyzer%result%total_lines = 4
        analyzer%analysis_complete = .true.
        
        line_text = analyzer%get_line_text(3)
        
        if (line_text == "  x = 42") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '  x = 42'"
            print *, "  Got: '", line_text, "'"
        end if
    end block

    ! Test 3: Source context line starts calculation
    call test_start("Source context line starts calculation")
    block
        type(source_context_t) :: context
        character(*), parameter :: test_source = &
            "line1" // new_line('a') // &
            "line2" // new_line('a') // &
            "line3"
        
        call context%initialize_source(test_source)
        
        if (context%total_lines == 3 .and. &
            context%line_starts(1) == 1 .and. &
            context%line_starts(2) == 7 .and. &
            context%line_starts(3) == 13) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: total_lines=3, starts=[1,7,13]"
            print *, "  Got: total_lines=", context%total_lines
            if (allocated(context%line_starts)) then
                print *, "  line_starts=", context%line_starts
            end if
        end if
    end block

    ! Test 4: Extract range with bounds checking
    call test_start("Extract range with bounds checking")
    block
        type(source_context_t) :: context
        character(*), parameter :: test_source = "hello world"
        type(source_location_t) :: location
        character(:), allocatable :: extracted
        
        call context%initialize_source(test_source)
        
        ! Valid range
        location = source_location_t(line=1, column=1, start_char=1, end_char=5)
        extracted = context%extract_range(location)
        
        if (extracted == "hello") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'hello'"
            print *, "  Got: '", extracted, "'"
        end if
    end block

    ! Test 5: Invalid range bounds checking
    call test_start("Invalid range bounds checking")
    block
        type(source_context_t) :: context
        character(*), parameter :: test_source = "hello world"
        type(source_location_t) :: location
        character(:), allocatable :: extracted
        
        call context%initialize_source(test_source)
        
        ! Invalid range (end before start)
        location = source_location_t(line=1, column=1, start_char=5, end_char=3)
        extracted = context%extract_range(location)
        
        if (extracted == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for invalid range"
            print *, "  Got: '", extracted, "'"
        end if
    end block

    ! Test 6: Out of bounds character positions
    call test_start("Out of bounds character positions")
    block
        type(source_context_t) :: context
        character(*), parameter :: test_source = "hello"
        type(source_location_t) :: location
        character(:), allocatable :: extracted
        
        call context%initialize_source(test_source)
        
        ! Beyond source length
        location = source_location_t(line=1, column=1, start_char=1, end_char=20)
        extracted = context%extract_range(location)
        
        if (extracted == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for out of bounds"
            print *, "  Got: '", extracted, "'"
        end if
    end block

    ! Test 7: Empty source handling
    call test_start("Empty source handling")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        character(:), allocatable :: line_text
        
        ! Uninitialized analyzer
        analyzer%analysis_complete = .false.
        
        line_text = analyzer%get_line_text(1)
        
        if (line_text == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for uninitialized analyzer"
            print *, "  Got: '", line_text, "'"
        end if
    end block

    ! Test 8: Line number validation
    call test_start("Line number validation")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        character(*), parameter :: test_source = "line1" // new_line('a') // "line2"
        character(:), allocatable :: line_text
        
        analyzer%result%original_source = test_source
        analyzer%result%total_lines = 2
        analyzer%analysis_complete = .true.
        
        ! Invalid line number
        line_text = analyzer%get_line_text(5)
        
        if (line_text == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for invalid line number"
            print *, "  Got: '", line_text, "'"
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All comprehensive source reconstruction tests passed!"
        stop 0
    else
        print *, "Some comprehensive source reconstruction tests failed!"
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

end program test_source_reconstruction_comprehensive