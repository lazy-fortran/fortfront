module standardizer_program
    ! Program-specific transformations module
    ! Handles program node standardization, contains insertion, and analysis
    
    use ast_core
    use ast_factory
    use type_system_unified
    use error_handling
    use standardizer_declarations
    use standardizer_allocatable
    use standardizer_subprograms, only: standardize_subprograms
    implicit none
    private

    public :: standardize_program
    public :: analyze_program_content
    public :: find_contains_insertion_point
    public :: insert_contains_statement
    

contains

    ! Standardize a program node
    subroutine standardize_program(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        logical :: has_functions, has_subroutines, has_use_statements
        logical :: has_executable_statements
        logical :: should_be_module
        integer :: contains_index
        integer, allocatable :: new_body_indices(:)
        integer :: i, n_statements, insert_pos

        ! Analyze the program to determine if it should be a module
        call analyze_program_content(arena, prog, has_functions, has_subroutines, &
                        has_use_statements, has_executable_statements, should_be_module)

        if (should_be_module) then
            ! TODO: Transform to module_node
            ! For now, just handle contains insertion
        end if

        ! Standardize existing declarations (e.g., real -> real(8))
        call standardize_declarations(arena, prog)

        ! Always insert implicit none and variable declarations for programs
        call insert_variable_declarations(arena, prog, prog_index)
        
        ! Mark variables that need allocatable due to array reassignment (Issue 188)
        call mark_allocatable_for_array_reassignments(arena, prog, prog_index)
        
        ! Mark variables that need allocatable due to string length changes (Issue 218)
        call mark_allocatable_for_string_length_changes(arena, prog)

        ! Check if we need to insert a contains statement
        if (has_functions .or. has_subroutines) then
            ! Find where to insert contains (before first function/subroutine)
            insert_pos = find_contains_insertion_point(arena, prog)

            if (insert_pos > 0) then
                ! Create new body with contains statement
                call insert_contains_statement(arena, prog, prog_index, insert_pos)
            end if
        end if

        ! Standardize function and subroutine definitions
        call standardize_subprograms(arena, prog)

    end subroutine standardize_program

    ! Analyze program content to determine its nature
    subroutine analyze_program_content(arena, prog, has_functions, has_subroutines, &
                        has_use_statements, has_executable_statements, should_be_module)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical, intent(out) :: has_functions, has_subroutines, has_use_statements
        logical, intent(out) :: has_executable_statements, should_be_module
        integer :: i

        has_functions = .false.
        has_subroutines = .false.
        has_use_statements = .false.
        has_executable_statements = .false.
        should_be_module = .false.

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        has_functions = .true.
                    type is (subroutine_def_node)
                        has_subroutines = .true.
                    type is (use_statement_node)
                        has_use_statements = .true.
                    type is (assignment_node)
                        has_executable_statements = .true.
                    type is (call_or_subscript_node)
                        has_executable_statements = .true.
                    type is (subroutine_call_node)
                        has_executable_statements = .true.
                    type is (print_statement_node)
                        has_executable_statements = .true.
                    type is (if_node)
                        has_executable_statements = .true.
                    type is (do_loop_node)
                        has_executable_statements = .true.
                    type is (do_while_node)
                        has_executable_statements = .true.
                    type is (select_case_node)
                        has_executable_statements = .true.
                    end select
                end if
            end if
        end do

        ! Decision logic: if only functions/subroutines and use statements, 
        ! it's likely a module
        ! For now, keep it simple - presence of multiple procedures suggests module
        should_be_module = (has_functions .or. has_subroutines) .and. &
                   (count([has_functions, has_subroutines]) > 1 .or. has_use_statements)

    end subroutine analyze_program_content

    ! Find where to insert the contains statement
    function find_contains_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos
        integer :: i

        pos = 0
        if (.not. allocated(prog%body_indices)) return

        ! Find the first function or subroutine
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        pos = i
                        return
                    type is (subroutine_def_node)
                        pos = i
                        return
                    end select
                end if
            end if
        end do

    end function find_contains_insertion_point

    ! Insert a contains statement at the specified position
    subroutine insert_contains_statement(arena, prog, prog_index, insert_pos)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index, insert_pos
        integer, allocatable :: new_body_indices(:)
        integer :: contains_index, i, j
        type(contains_node) :: contains_stmt

        if (.not. allocated(prog%body_indices)) return

        ! Create contains node
        contains_stmt%line = 1  ! Line number will be adjusted later
        contains_stmt%column = 1

        ! Add contains node to arena
        call arena%push(contains_stmt, "contains", prog_index)
        contains_index = arena%size

        ! Create new body indices array with contains inserted
        allocate (new_body_indices(size(prog%body_indices) + 1))

        ! Copy statements before the insertion point
        j = 1
        do i = 1, insert_pos - 1
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Insert contains
        new_body_indices(j) = contains_index
        j = j + 1

        ! Copy remaining statements
        do i = insert_pos, size(prog%body_indices)
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Replace body indices
        prog%body_indices = new_body_indices

        ! Update the arena entry
        arena%entries(prog_index)%node = prog

    end subroutine insert_contains_statement

end module standardizer_program