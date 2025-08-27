program debug_boundary_issue
    use frontend_parsing
    use lexer_core
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    integer :: i, unit_start, unit_end
    logical :: has_explicit_program_unit
    
    ! Test with just the type definition
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t'
    
    print *, "=== Debug Boundary Issue ==="
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    print *, "Tokens:"
    do i = 1, min(15, size(tokens))
        print *, i, ": ", trim(tokens(i)%text), " (", tokens(i)%kind, ")"
    end do
    print *, ""
    
    ! Test boundary detection
    has_explicit_program_unit = .false.
    
    print *, "Testing boundary detection from position 1:"
    call find_program_unit_boundary(tokens, 1, unit_start, unit_end, &
                                   has_explicit_program_unit)
    print *, "Boundary: start=", unit_start, ", end=", unit_end
    print *, "Tokens in this unit:"
    do i = unit_start, min(unit_end, size(tokens))
        if (i >= 1 .and. i <= size(tokens)) then
            print *, "  ", i, ": ", trim(tokens(i)%text)
        end if
    end do
    
    if (unit_end < size(tokens)) then
        print *, ""
        print *, "Testing boundary detection from position ", unit_end + 1, ":"
        call find_program_unit_boundary(tokens, unit_end + 1, unit_start, unit_end, &
                                       has_explicit_program_unit)
        print *, "Boundary: start=", unit_start, ", end=", unit_end
    end if
    
end program debug_boundary_issue