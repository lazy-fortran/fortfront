program test_intent_inference_subroutine
    implicit none
    character(len=:), allocatable :: source
    integer, parameter :: unit = 42
    logical :: exists_flag
    integer :: ios
    character(len=1000) :: line
    logical :: found_inout, found_in
    integer :: inout_count

    print *, "=== Testing Subroutine Intent Inference (Issue #2114) ==="

    call read_example('examples/lf/issue_missing_intent_inout_subroutine.lf', &
                     source)
    call execute_command_line('./build/gfortran_*/app/fortfront ' // &
                             'examples/lf/issue_missing_intent_inout_subroutine.lf ' // &
                             '> /tmp/test_intent_2114.f90')

    found_inout = .false.
    inquire (file='/tmp/test_intent_2114.f90', exist=exists_flag)
    if (.not. exists_flag) then
        print *, "FAIL: Output file not created"
        stop 1
    end if

    open (unit, file='/tmp/test_intent_2114.f90', status='old', iostat=ios)
    if (ios /= 0) then
        print *, "FAIL: Cannot open output file"
        stop 1
    end if

    do
        read (unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'intent(inout)') > 0) then
            found_inout = .true.
            exit
        end if
    end do
    close (unit)

    if (found_inout) then
        print *, "PASS: intent(inout) inferred for modified subroutine parameter"
    else
        print *, "FAIL: intent(inout) not found in output"
        stop 1
    end if

    call test_intent_in_only()
    call test_mixed_intents()

    print *, "All subroutine intent inference tests PASSED"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, ios
        character(len=:), allocatable :: temp_content

        inquire (file=filepath, size=file_size)
        allocate (character(len=file_size) :: temp_content)

        open (newunit=unit, file=filepath, status='old', action='read', &
             iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Cannot open file: ", filepath
            stop 1
        end if

        read (unit, '(A)', iostat=ios) temp_content
        close (unit)
        content = trim(temp_content)
    end subroutine read_example

    subroutine test_intent_in_only()
        integer :: unit, ios
        character(len=1000) :: line
        logical :: found

        call execute_command_line('echo "subroutine print_val(x)" > /tmp/test_in.lf')
        call execute_command_line('echo "    print *, x" >> /tmp/test_in.lf')
        call execute_command_line('echo "end subroutine" >> /tmp/test_in.lf')
        call execute_command_line('./build/gfortran_*/app/fortfront ' // &
                                 '/tmp/test_in.lf > /tmp/test_in_out.f90')

        found = .false.
        open (newunit=unit, file='/tmp/test_in_out.f90', status='old')
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'intent(in)') > 0) then
                found = .true.
                exit
            end if
        end do
        close (unit)

        if (found) then
            print *, "PASS: intent(in) inferred for read-only parameter"
        else
            print *, "FAIL: intent(in) not found"
            stop 1
        end if
    end subroutine test_intent_in_only

    subroutine test_mixed_intents()
        integer :: unit, ios
        character(len=1000) :: line
        integer :: inout_count, in_count

        call execute_command_line('echo "subroutine process(a,b,c)" > /tmp/test_mixed.lf')
        call execute_command_line('echo "    a = a + 1" >> /tmp/test_mixed.lf')
        call execute_command_line('echo "    b = 5" >> /tmp/test_mixed.lf')
        call execute_command_line('echo "    print *, c" >> /tmp/test_mixed.lf')
        call execute_command_line('echo "end subroutine" >> /tmp/test_mixed.lf')
        call execute_command_line('./build/gfortran_*/app/fortfront ' // &
                                 '/tmp/test_mixed.lf > /tmp/test_mixed_out.f90')

        inout_count = 0
        in_count = 0
        open (newunit=unit, file='/tmp/test_mixed_out.f90', status='old')
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'intent(inout)') > 0) inout_count = inout_count + 1
            if (index(line, 'intent(in)') > 0) in_count = in_count + 1
        end do
        close (unit)

        if (inout_count == 2 .and. in_count == 1) then
            print *, "PASS: Mixed intents inferred correctly (2 inout, 1 in)"
        else
            print *, "FAIL: Expected 2 inout and 1 in, got:", &
                     inout_count, "inout and", in_count, "in"
            stop 1
        end if
    end subroutine test_mixed_intents

end program test_intent_inference_subroutine
