program test_do_loop_issue_620
    ! Test that do loops generate proper code, not TODO placeholders (Issue #620)
    use frontend, only: compile_source, compilation_options_t
    use iso_fortran_env, only: error_unit
    implicit none
    
    logical :: all_passed
    integer :: unit, iostat
    character(len=256) :: line
    logical :: found_do, found_todo
    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    
    print *, "=== Testing Do Loop Code Generation (Issue #620) ==="
    
    ! Create test input with do loop
    input_file = 'test_do_issue_620.f90'
    open(newunit=unit, file=input_file, status='replace')
    write(unit, '(a)') 'program test'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: i'
    write(unit, '(a)') '  do i = 1, 3'
    write(unit, '(a)') '    print *, i'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') 'end program test'
    close(unit)
    
    ! Compile with frontend
    output_file = 'test_do_issue_620_out.f90'
    options%output_file = output_file
    
    call compile_source(input_file, options, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: Compilation error:', trim(error_msg)
        stop 1
    end if
    
    ! Check generated code
    found_do = .false.
    found_todo = .false.
    
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
        print *, 'FAIL: Cannot open output file'
        stop 1
    end if
    
    do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        
        ! Check for proper do loop generation
        if (index(line, 'do i = 1, 3') > 0) then
            found_do = .true.
            print *, 'Found do loop:', trim(line)
        end if
        
        ! Check for TODO placeholder (should NOT be present)
        if (index(line, 'TODO') > 0 .and. index(line, 'codegen') > 0) then
            found_todo = .true.
            print *, 'ERROR - Found TODO placeholder:', trim(line)
        end if
    end do
    
    close(unit)
    
    ! Report results
    if (found_todo) then
        print *, 'FAIL: TODO placeholder found - Issue #620 NOT FIXED'
        stop 1
    else if (.not. found_do) then
        print *, 'FAIL: Do loop not generated properly'
        stop 1
    else
        print *, 'PASS: Do loop generates proper code - Issue #620 FIXED!'
        stop 0
    end if
    
end program test_do_loop_issue_620