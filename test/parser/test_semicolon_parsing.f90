program test_semicolon_parsing
    ! Test semicolon-separated statement parsing
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed
    
    print *, "=== Semicolon-separated Statement Parsing Tests ==="
    
    all_passed = test_simple_semicolons()
    all_passed = all_passed .and. test_complex_semicolons()
    all_passed = all_passed .and. test_mixed_semicolons()
    
    if (all_passed) then
        print *, "All semicolon parsing tests passed!"
        stop 0
    else
        print *, "Some semicolon parsing tests failed!"
        stop 1
    end if

contains

    logical function test_simple_semicolons()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: found_assignments
        
        test_simple_semicolons = .true.
        print *, "Testing simple semicolon-separated assignments..."
        
        ! Create test input with semicolon-separated statements
        input_file = 'test_semicolons_simple.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'integer :: a, b, c; a = 1; b = 2; c = 3'
        write(unit, '(a)') 'write(*,*) a, b, c'
        close(unit)
        
        ! Compile with frontend
        output_file = 'test_semicolons_simple_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_simple_semicolons = .false.
            return
        end if
        
        ! Check generated code contains all assignments
        call check_generated_assignments(output_file, found_assignments)
        
        if (found_assignments) then
            print *, '  PASS: All semicolon-separated assignments found'
        else
            print *, '  FAIL: Missing assignments in generated code'
            test_simple_semicolons = .false.
        end if
        
    end function test_simple_semicolons

    logical function test_complex_semicolons()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: found_statements
        
        test_complex_semicolons = .true.
        print *, "Testing complex semicolon-separated statements..."
        
        ! Create test input with various statement types
        input_file = 'test_semicolons_complex.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'integer :: i; real :: x; i = 42; x = 3.14'
        write(unit, '(a)') 'print *, i; print *, x'
        close(unit)
        
        ! Compile with frontend
        output_file = 'test_semicolons_complex_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_complex_semicolons = .false.
            return
        end if
        
        ! Check generated code contains expected statements
        call check_generated_statements(output_file, found_statements)
        
        if (found_statements) then
            print *, '  PASS: All complex semicolon statements found'
        else
            print *, '  FAIL: Missing statements in generated code'
            test_complex_semicolons = .false.
        end if
        
    end function test_complex_semicolons
    
    logical function test_mixed_semicolons()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: found_mixed_content
        
        test_mixed_semicolons = .true.
        print *, "Testing mixed semicolon and regular statements..."
        
        ! Create test input mixing semicolon and regular statements
        input_file = 'test_semicolons_mixed.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'integer :: a; a = 100'
        write(unit, '(a)') 'integer :: b'
        write(unit, '(a)') 'b = 200; print *, a; print *, b'
        close(unit)
        
        ! Compile with frontend
        output_file = 'test_semicolons_mixed_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_mixed_semicolons = .false.
            return
        end if
        
        ! Check generated code contains expected mixed content
        call check_generated_mixed_content(output_file, found_mixed_content)
        
        if (found_mixed_content) then
            print *, '  PASS: Mixed semicolon and regular statements found'
        else
            print *, '  FAIL: Missing mixed content in generated code'
            test_mixed_semicolons = .false.
        end if
        
    end function test_mixed_semicolons

    subroutine check_generated_assignments(output_file, found_assignments)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: found_assignments
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_a, found_b, found_c
        
        found_assignments = .false.
        found_a = .false.
        found_b = .false.
        found_c = .false.
        
        open(newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, 'a = 1') > 0) found_a = .true.
            if (index(line, 'b = 2') > 0) found_b = .true.
            if (index(line, 'c = 3') > 0) found_c = .true.
        end do
        
        close(unit)
        found_assignments = found_a .and. found_b .and. found_c
        
    end subroutine check_generated_assignments

    subroutine check_generated_statements(output_file, found_statements)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: found_statements
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_i_decl, found_x_decl, found_i_assign, found_x_assign
        
        found_statements = .false.
        found_i_decl = .false.
        found_x_decl = .false.
        found_i_assign = .false.
        found_x_assign = .false.
        
        open(newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, 'integer') > 0 .and. index(line, 'i') > 0) found_i_decl = .true.
            if (index(line, 'real') > 0 .and. index(line, 'x') > 0) found_x_decl = .true.
            if (index(line, 'i = 42') > 0) found_i_assign = .true.
            if (index(line, 'x = 3.14') > 0) found_x_assign = .true.
        end do
        
        close(unit)
        found_statements = found_i_decl .and. found_x_decl .and. found_i_assign .and. found_x_assign
        
    end subroutine check_generated_statements
    
    subroutine check_generated_mixed_content(output_file, found_mixed_content)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: found_mixed_content
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_a_assign, found_b_decl, found_b_assign
        
        found_mixed_content = .false.
        found_a_assign = .false.
        found_b_decl = .false.
        found_b_assign = .false.
        
        open(newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, 'a = 100') > 0) found_a_assign = .true.
            if (index(line, 'integer') > 0 .and. index(line, 'b') > 0) found_b_decl = .true.
            if (index(line, 'b = 200') > 0) found_b_assign = .true.
        end do
        
        close(unit)
        found_mixed_content = found_a_assign .and. found_b_decl .and. found_b_assign
        
    end subroutine check_generated_mixed_content

end program test_semicolon_parsing