module standardizer_program
    ! Program-specific transformations module
    ! Handles program node standardization, contains insertion, and analysis

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_procedure
    use ast_nodes_misc
    use ast_nodes_io
    use ast_nodes_control
    use ast_factory
    use type_system_unified
    use error_handling
    use standardizer_declarations
    use standardizer_allocatable
    use standardizer_subprograms, only: standardize_subprograms
    use standardizer_parameter, only: get_standardizer_input_mode
    use semantic_input_mode, only: INPUT_MODE_STANDARD
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
        logical :: is_standard

        is_standard = (get_standardizer_input_mode() == INPUT_MODE_STANDARD)

        ! Analyze the program to determine if it should be a module
        call analyze_program_content(arena, prog, has_functions, &
                                     has_subroutines, has_use_statements, &
                                     has_executable_statements, should_be_module)

        if (should_be_module) then
            ! Handle contains insertion for module-like programs
            ! Find where to insert contains (before first function/subroutine)
            insert_pos = find_contains_insertion_point(arena, prog)
            if (insert_pos > 0) then
                ! Ensure contains is present exactly once in the correct position
                call ensure_contains_before_index(arena, prog, prog_index, &
                                                  insert_pos)
            end if
        end if

        ! For standard Fortran input, skip declaration-related standardization
        ! to preserve original declarations, implicit rules, and formatting.
        ! However, still handle contains insertion for wrapped subprograms.
        if (is_standard) then
            ! Check if we need to insert a contains statement
            if (has_functions .or. has_subroutines) then
                ! Find where to insert contains (before first function/subroutine)
                insert_pos = find_contains_insertion_point(arena, prog)

                if (insert_pos > 0) then
                    ! Ensure contains is present exactly once in the correct position
                    call ensure_contains_before_index(arena, prog, prog_index, &
                                                      insert_pos)
                end if
            end if

            ! For standard mode, skip subprogram standardization to preserve
            ! original formatting and declarations
            return
        end if

        ! Standardize existing declarations (e.g., real -> real(dp))
        call standardize_declarations(arena, prog)

        ! First, mark allocatable needs and split multi-declarations as needed
        call mark_allocatable_for_array_reassignments(arena, prog, prog_index)
        call mark_allocatable_for_string_length_changes(arena, prog)

        ! Then insert implicit none and any missing variable declarations
        call insert_variable_declarations(arena, prog, prog_index)

        ! Re-run declaration standardization to cover newly inserted declarations
        call standardize_declarations(arena, prog)

        ! Check if we need to insert a contains statement
        if (has_functions .or. has_subroutines) then
            ! Find where to insert contains (before first function/subroutine)
            insert_pos = find_contains_insertion_point(arena, prog)

            if (insert_pos > 0) then
                ! Ensure contains is present exactly once in the correct position
                call ensure_contains_before_index(arena, prog, prog_index, &
                                                  insert_pos)
            end if
        end if

        ! Standardize function and subroutine definitions
        call standardize_subprograms(arena, prog)

    end subroutine standardize_program

    ! Analyze program content to determine its nature
    subroutine analyze_program_content(arena, prog, has_functions, has_subroutines, &
                                       has_use_statements, has_executable_statements, &
                                       should_be_module)
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
                    type is (pointer_assignment_node)
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
                    type is (nullify_node)
                        has_executable_statements = .true.
                    end select
                end if
            end if
        end do

        ! A wrapped program that contains any procedure needs a `contains`
        ! separator before the first procedure. This flag drives that early
        ! insertion; the result is still emitted as a program, not a module
        ! (the name "should_be_module" is historical). Any procedure present is
        ! the precise condition, so guessing module-ness from procedure counts
        ! or use statements is neither needed nor accurate.
        should_be_module = has_functions .or. has_subroutines

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

    subroutine ensure_contains_before_index(arena, prog, prog_index, insert_pos)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index, insert_pos
        integer :: existing_pos

        if (insert_pos <= 0) return
        existing_pos = find_existing_contains_position(arena, prog)

        if (existing_pos > 0) then
            if (existing_pos < insert_pos) then
                return
            end if
            call reposition_contains_statement(arena, prog, prog_index, &
                                               existing_pos, insert_pos)
        else
            call insert_contains_statement(arena, prog, prog_index, insert_pos)
        end if
    end subroutine ensure_contains_before_index

    integer function find_existing_contains_position(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: i, idx

        pos = 0
        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (contains_node)
                pos = i
                return
            end select
        end do
    end function find_existing_contains_position

    subroutine reposition_contains_statement(arena, prog, prog_index, &
                                             current_pos, insert_pos)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index, current_pos, insert_pos
        integer, allocatable :: without_contains(:)
        integer, allocatable :: final_indices(:)
        integer :: contains_idx
        integer :: target_pos
        integer :: i, j

        if (.not. allocated(prog%body_indices)) return
        if (current_pos <= 0 .or. current_pos > size(prog%body_indices)) return

        contains_idx = prog%body_indices(current_pos)

        if (current_pos < insert_pos) then
            return
        end if

        allocate (without_contains(size(prog%body_indices) - 1))
        j = 1
        do i = 1, size(prog%body_indices)
            if (i == current_pos) cycle
            without_contains(j) = prog%body_indices(i)
            j = j + 1
        end do

        target_pos = insert_pos
        if (target_pos < 1) target_pos = 1
        if (target_pos > size(without_contains) + 1) &
            target_pos = size(without_contains) + 1

        allocate (final_indices(size(without_contains) + 1))
        j = 1
        do i = 1, target_pos - 1
            final_indices(j) = without_contains(i)
            j = j + 1
        end do
        final_indices(j) = contains_idx
        j = j + 1
        do i = target_pos, size(without_contains)
            final_indices(j) = without_contains(i)
            j = j + 1
        end do

        block
            integer, allocatable :: tmp(:)
            call move_alloc(prog%body_indices, tmp)
        end block
        call move_alloc(final_indices, prog%body_indices)

        arena%entries(prog_index)%node = prog
    end subroutine reposition_contains_statement

end module standardizer_program
