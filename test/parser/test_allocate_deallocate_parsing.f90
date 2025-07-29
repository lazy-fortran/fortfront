program test_allocate_deallocate_parsing
    use frontend, only: compile_source, compilation_options_t
    use ast_core
    implicit none

    logical :: all_tests_passed

    print *, "=== Allocate/Deallocate Parsing Tests (RED Phase) ==="
    print *

    all_tests_passed = .true.

    ! Run all tests
    if (.not. test_simple_allocate_parsing()) all_tests_passed = .false.
    if (.not. test_array_allocate_parsing()) all_tests_passed = .false.
    if (.not. test_allocate_with_stat_parsing()) all_tests_passed = .false.
    if (.not. test_simple_deallocate_parsing()) all_tests_passed = .false.
    if (.not. test_deallocate_with_stat_parsing()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All allocate/deallocate parsing tests passed!"
        stop 0
    else
        print *, "Some allocate/deallocate parsing tests failed (expected in RED phase)!"
        stop 1
    end if

contains

    function test_simple_allocate_parsing() result(passed)
        logical :: passed
        type(compilation_options_t) :: opts
        type(ast_arena_t) :: arena
        character(len=256) :: source_file
        character(len=1024) :: error_msg
        integer :: prog_idx

        passed = .false.
        
        ! Create simple allocate test program
        source_file = 'simple_allocate.f90'
        open(unit=10, file=source_file, status='replace')
        write(10, '(A)') 'program test'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer, pointer :: p'
        write(10, '(A)') '    allocate(p)'
        write(10, '(A)') 'end program test'
        close(10)

        ! Try to parse the source
        call compile_source(source_file, opts, error_msg)

        if (len_trim(error_msg) == 0 .and. prog_idx > 0) then
            ! Check if allocate statement was parsed correctly  
            if (contains_allocate_statement(arena, prog_idx)) then
                print *, "PASS: Simple allocate statement parsed successfully"
                passed = .true.
            else
                print *, "FAIL: Allocate statement not found in AST"
            end if
        else
            print *, "EXPECTED FAIL: Parser error for allocate statement"
            print *, "  Error: ", trim(error_msg)
            print *, "  This confirms allocate parsing is not implemented yet"
            ! In RED phase, this is expected to fail
        end if
    end function test_simple_allocate_parsing

    function test_array_allocate_parsing() result(passed)
        logical :: passed
        type(compilation_options_t) :: opts
        type(ast_arena_t) :: arena
        character(len=256) :: source_file
        character(len=1024) :: error_msg
        integer :: prog_idx

        passed = .false.
        
        ! Create array allocate test program
        source_file = 'array_allocate.f90'
        open(unit=10, file=source_file, status='replace')
        write(10, '(A)') 'program test'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer, allocatable :: arr(:)'
        write(10, '(A)') '    allocate(arr(100))'
        write(10, '(A)') 'end program test'
        close(10)

        ! Try to parse the source
        call compile_source(source_file, opts, error_msg)

        if (len_trim(error_msg) == 0 .and. prog_idx > 0) then
            ! Check if array allocate statement was parsed correctly  
            if (contains_allocate_statement(arena, prog_idx)) then
                print *, "PASS: Array allocate statement parsed successfully"
                passed = .true.
            else
                print *, "FAIL: Array allocate statement not found in AST"
            end if
        else
            print *, "EXPECTED FAIL: Parser error for array allocate statement"
            print *, "  Error: ", trim(error_msg)
            print *, "  This confirms array allocate parsing is not implemented yet"
            ! In RED phase, this is expected to fail
        end if
    end function test_array_allocate_parsing

    function test_allocate_with_stat_parsing() result(passed)
        logical :: passed
        type(compilation_options_t) :: opts
        type(ast_arena_t) :: arena
        character(len=256) :: source_file
        character(len=1024) :: error_msg
        integer :: prog_idx

        passed = .false.
        
        ! Create allocate with stat test program
        source_file = 'allocate_stat.f90'
        open(unit=10, file=source_file, status='replace')
        write(10, '(A)') 'program test'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer, pointer :: p'
        write(10, '(A)') '    integer :: stat'
        write(10, '(A)') '    allocate(p, stat=stat)'
        write(10, '(A)') 'end program test'
        close(10)

        ! Try to parse the source
        call compile_source(source_file, opts, error_msg)

        if (len_trim(error_msg) == 0 .and. prog_idx > 0) then
            ! Check if allocate with stat was parsed correctly  
            if (contains_allocate_statement(arena, prog_idx)) then
                print *, "PASS: Allocate with stat statement parsed successfully"
                passed = .true.
            else
                print *, "FAIL: Allocate with stat statement not found in AST"
            end if
        else
            print *, "EXPECTED FAIL: Parser error for allocate with stat statement"
            print *, "  Error: ", trim(error_msg)
            print *, "  This confirms allocate with stat parsing is not implemented yet"
            ! In RED phase, this is expected to fail
        end if
    end function test_allocate_with_stat_parsing

    function test_simple_deallocate_parsing() result(passed)
        logical :: passed
        type(compilation_options_t) :: opts
        type(ast_arena_t) :: arena
        character(len=256) :: source_file
        character(len=1024) :: error_msg
        integer :: prog_idx

        passed = .false.
        
        ! Create simple deallocate test program
        source_file = 'simple_deallocate.f90'
        open(unit=10, file=source_file, status='replace')
        write(10, '(A)') 'program test'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer, pointer :: p'
        write(10, '(A)') '    deallocate(p)'
        write(10, '(A)') 'end program test'
        close(10)

        ! Try to parse the source
        call compile_source(source_file, opts, error_msg)

        if (len_trim(error_msg) == 0 .and. prog_idx > 0) then
            ! Check if deallocate statement was parsed correctly  
            if (contains_deallocate_statement(arena, prog_idx)) then
                print *, "PASS: Simple deallocate statement parsed successfully"
                passed = .true.
            else
                print *, "FAIL: Deallocate statement not found in AST"
            end if
        else
            print *, "EXPECTED FAIL: Parser error for deallocate statement"
            print *, "  Error: ", trim(error_msg)
            print *, "  This confirms deallocate parsing is not implemented yet"
            ! In RED phase, this is expected to fail
        end if
    end function test_simple_deallocate_parsing

    function test_deallocate_with_stat_parsing() result(passed)
        logical :: passed
        type(compilation_options_t) :: opts
        type(ast_arena_t) :: arena
        character(len=256) :: source_file
        character(len=1024) :: error_msg
        integer :: prog_idx

        passed = .false.
        
        ! Create deallocate with stat test program
        source_file = 'deallocate_stat.f90'
        open(unit=10, file=source_file, status='replace')
        write(10, '(A)') 'program test'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer, pointer :: p'
        write(10, '(A)') '    integer :: stat'
        write(10, '(A)') '    deallocate(p, stat=stat)'
        write(10, '(A)') 'end program test'
        close(10)

        ! Try to parse the source
        call compile_source(source_file, opts, error_msg)

        if (len_trim(error_msg) == 0 .and. prog_idx > 0) then
            ! Check if deallocate with stat was parsed correctly  
            if (contains_deallocate_statement(arena, prog_idx)) then
                print *, "PASS: Deallocate with stat statement parsed successfully"
                passed = .true.
            else
                print *, "FAIL: Deallocate with stat statement not found in AST"
            end if
        else
            print *, "EXPECTED FAIL: Parser error for deallocate with stat statement"
            print *, "  Error: ", trim(error_msg)
            print *, "  This confirms deallocate with stat parsing is not implemented yet"
            ! In RED phase, this is expected to fail
        end if
    end function test_deallocate_with_stat_parsing

    ! Helper function to check if AST contains allocate statement
    function contains_allocate_statement(arena, prog_idx) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_idx
        logical :: found
        integer :: i

        found = .false.
        
        ! Search through the AST for allocate_statement_node
        do i = 1, arena%size
            if (arena%entries(i)%node_type == "allocate_statement_node") then
                found = .true.
                return
            end if
        end do
    end function contains_allocate_statement

    ! Helper function to check if AST contains deallocate statement
    function contains_deallocate_statement(arena, prog_idx) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_idx
        logical :: found
        integer :: i

        found = .false.
        
        ! Search through the AST for deallocate_statement_node
        do i = 1, arena%size
            if (arena%entries(i)%node_type == "deallocate_statement_node") then
                found = .true.
                return
            end if
        end do
    end function contains_deallocate_statement

end program test_allocate_deallocate_parsing