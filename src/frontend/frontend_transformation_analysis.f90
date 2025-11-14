module frontend_transformation_analysis
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, identifier_node, call_or_subscript_node, &
                              assignment_node, component_access_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node
    use ast_nodes_data, only: module_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: build_procedure_membership
    public :: analyze_ast_content
    public :: analyze_single_unit
    public :: collect_host_assignment_names
    public :: collect_program_assignment_names
    public :: collect_procedure_assignment_names
    public :: collect_assignment_from_node
    public :: record_identifier_name
    public :: append_unique_name
    public :: promote_functions_to_internal_program
    public :: requires_lazy_internalization
    public :: has_existing_module_in_ast

contains
    recursive subroutine mark_procedure_subtree(arena, node_index, membership)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(inout) :: membership(:)
        integer :: child_idx, child_count, member_idx

        if (node_index <= 0) return
        if (node_index > size(membership)) return
        if (membership(node_index)) return

        membership(node_index) = .true.

        if (.not. allocated(arena%entries(node_index)%child_indices)) return
        child_count = size(arena%entries(node_index)%child_indices)
        if (child_count == 0) return
        do child_idx = 1, child_count
            member_idx = arena%entries(node_index)%child_indices(child_idx)
            call mark_procedure_subtree(arena, member_idx, membership)
        end do
    end subroutine mark_procedure_subtree

    subroutine build_procedure_membership(arena, membership)
        type(ast_arena_t), intent(in) :: arena
        logical, allocatable, intent(out) :: membership(:)
        integer :: i, j, body_idx

        if (arena%size <= 0) then
            allocate (membership(0))
            return
        end if

        allocate (membership(arena%size))
        membership = .false.

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (proc => arena%entries(i)%node)
            type is (function_def_node)
                if (allocated(proc%body_indices)) then
                    do j = 1, size(proc%body_indices)
                        body_idx = proc%body_indices(j)
                        call mark_procedure_subtree(arena, body_idx, membership)
                    end do
                end if
            type is (subroutine_def_node)
                if (allocated(proc%body_indices)) then
                    do j = 1, size(proc%body_indices)
                        body_idx = proc%body_indices(j)
                        call mark_procedure_subtree(arena, body_idx, membership)
                    end do
                end if
            end select
        end do
    end subroutine build_procedure_membership

    ! Analyze AST content directly (no string manipulation)

    subroutine analyze_ast_content(arena, root_index, has_functions, &
                                   has_subroutines, has_main_code)
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        logical, intent(out) :: has_functions, has_subroutines, has_main_code
        integer :: i, j
        logical, allocatable :: in_procedure(:)

        has_functions = .false.
        has_subroutines = .false.
        has_main_code = .false.

        ! Check if root is already a module - if so, don't wrap
        if (root_index > 0 .and. root_index <= arena%size) then
            if (allocated(arena%entries(root_index)%node)) then
                select type (root => arena%entries(root_index)%node)
                type is (module_node)
                    ! Already a module, no wrapping needed
                    return
                type is (program_node)
                    ! Check if this is a multi-unit container
                    if (root%name == "__MULTI_UNIT__" .and. &
                        allocated(root%body_indices)) then
                        ! Scan child units for functions, subroutines, and main code
                        do j = 1, size(root%body_indices)
                            call analyze_single_unit(arena, root%body_indices(j), &
                                                     has_functions, has_subroutines, &
                                                     has_main_code)
                        end do
                        return
                    end if
                end select
            end if
        end if

        ! For non-multi-unit roots, scan all nodes in arena
        call build_procedure_membership(arena, in_procedure)

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle

            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                has_functions = .true.
            type is (subroutine_def_node)
                has_subroutines = .true.
            type is (assignment_node)
                ! Assignment outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (print_statement_node)
                ! Print statement outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (if_node)
                ! Control flow outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (do_loop_node)
                ! Loop outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (subroutine_call_node)
                ! Subroutine call outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            end select
        end do
    end subroutine analyze_ast_content

    ! Analyze a single unit (program, function, or subroutine) for content
    subroutine analyze_single_unit(arena, unit_index, has_functions, &
                                   has_subroutines, has_main_code)
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: unit_index
        logical, intent(inout) :: has_functions, has_subroutines, has_main_code
        integer :: i

        if (unit_index <= 0 .or. unit_index > arena%size) return
        if (.not. allocated(arena%entries(unit_index)%node)) return

        select type (unit => arena%entries(unit_index)%node)
        type is (function_def_node)
            has_functions = .true.
        type is (subroutine_def_node)
            has_subroutines = .true.
        type is (program_node)
            ! Scan program body for executable statements
            if (allocated(unit%body_indices)) then
                do i = 1, size(unit%body_indices)
                    if (unit%body_indices(i) <= 0 .or. &
                        unit%body_indices(i) > arena%size) cycle
                    if (.not. allocated(arena%entries(unit%body_indices(i))%node)) &
                        cycle

                    select type (stmt => arena%entries(unit%body_indices(i))%node)
                    type is (assignment_node)
                        has_main_code = .true.
                    type is (print_statement_node)
                        has_main_code = .true.
                    type is (if_node)
                        has_main_code = .true.
                    type is (do_loop_node)
                        has_main_code = .true.
                    type is (subroutine_call_node)
                        has_main_code = .true.
                    type is (function_def_node)
                        has_functions = .true.
                    type is (subroutine_def_node)
                        has_subroutines = .true.
                    end select
                end do
            end if
        end select
    end subroutine analyze_single_unit

    subroutine promote_functions_to_internal_program(arena, root_index)
        use ast_nodes_core, only: program_node
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                        subroutine_call_node
        use ast_nodes_misc, only: contains_node, implicit_statement_node, &
                                  end_statement_node, comment_node, &
                                  directive_node, blank_line_node
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_data, only: mixed_construct_container_node, declaration_node
        use standardizer_program, only: insert_contains_statement
        use ast_factory, only: push_implicit_statement
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer :: i, child_index, main_prog_index, candidate_prog_index
        integer :: idx, body_size, n_proc, contains_pos, pos
        integer, allocatable :: proc_indices(:)
        integer, allocatable :: new_body(:)
        integer, allocatable :: main_stmts(:)
        integer, allocatable :: filtered_body(:)
        logical :: has_contains, child_is_main_candidate, has_exec, has_procs
        integer :: contains_index, implicit_none_index, prog_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        allocate (proc_indices(0))
        allocate (main_stmts(0))
        main_prog_index = 0
        candidate_prog_index = 0

        select type (root => arena%entries(root_index)%node)
        type is (mixed_construct_container_node)
            ! Handle mixed constructs: main statements plus explicit procedures
            if (allocated(root%implicit_declaration_indices)) then
                do i = 1, size(root%implicit_declaration_indices)
                    child_index = root%implicit_declaration_indices(i)
                    if (is_host_level_statement(arena, child_index)) then
                        main_stmts = [main_stmts, child_index]
                    end if
                end do
            end if
            if (allocated(root%explicit_program_indices)) then
                do i = 1, size(root%explicit_program_indices)
                    child_index = root%explicit_program_indices(i)
                    if (child_index <= 0 .or. child_index > arena%size) cycle
                    if (.not. allocated(arena%entries(child_index)%node)) cycle
                    select type (child => arena%entries(child_index)%node)
                    type is (function_def_node)
                        proc_indices = [proc_indices, child_index]
                    type is (subroutine_def_node)
                        proc_indices = [proc_indices, child_index]
                    end select
                end do
            end if

            ! Create a program with main code and internal procedures
            if (size(proc_indices) > 0) then
                if (size(main_stmts) == 0) then
                    ! No bare statements to wrap; keep procedures external
                    return
                end if
                ! Create program skeleton: implicit none + body + contains + procs
                implicit_none_index = push_implicit_statement(arena, .true., &
                     line=1, column=1, parent_index=0)

                ! Build program body: implicit none + main statements + procs
                body_size = 1 + size(main_stmts) + 1 + size(proc_indices)
                allocate (new_body(body_size))
                new_body(1) = implicit_none_index
                do i = 1, size(main_stmts)
                    new_body(1 + i) = main_stmts(i)
                end do

                ! Add contains statement
                block
                    type(contains_node) :: contains_stmt
                    contains_stmt%line = 1
                    contains_stmt%column = 1
                    call arena%push(contains_stmt, "contains", 0)
                    contains_index = arena%size
                end block
                new_body(1 + size(main_stmts) + 1) = contains_index

                ! Add procedures
                do i = 1, size(proc_indices)
                    new_body(1 + size(main_stmts) + 1 + i) = proc_indices(i)
                end do

                ! Create program node
                block
                    type(program_node) :: prog
                    prog%name = "main"
                    prog%body_indices = new_body
                    prog%line = 1
                    prog%column = 1
                    call arena%push(prog, "program", 0)
                    prog_index = arena%size
                end block

                ! Update parent indices
                arena%entries(implicit_none_index)%parent_index = prog_index
                do i = 1, size(main_stmts)
                    arena%entries(main_stmts(i))%parent_index = prog_index
                end do
                arena%entries(contains_index)%parent_index = prog_index
                do i = 1, size(proc_indices)
                    arena%entries(proc_indices(i))%parent_index = prog_index
                end do

                root_index = prog_index
                return
            end if

            return

        type is (program_node)
            if (trim(root%name) /= "__MULTI_UNIT__") return
            if (.not. allocated(root%body_indices)) return

            do i = 1, size(root%body_indices)
                child_index = root%body_indices(i)
                if (child_index <= 0 .or. child_index > arena%size) cycle
                if (.not. allocated(arena%entries(child_index)%node)) cycle

                select type (child => arena%entries(child_index)%node)
                type is (program_node)
                    child_is_main_candidate = .false.
                    has_exec = program_has_executable_statements(arena, &
                                                                child_index)
                    has_procs = program_contains_procedures(arena, child_index)
                    if (main_prog_index == 0) then
                        if (trim(child%name) /= "__MULTI_UNIT__") then
                            if (has_exec .and. .not. has_procs) then
                                main_prog_index = child_index
                                child_is_main_candidate = .true.
                            else if (has_exec .and. candidate_prog_index == 0) then
                                candidate_prog_index = child_index
                            end if
                        end if
                    else if (child_index == main_prog_index) then
                        child_is_main_candidate = .true.
                    end if
                    call collect_program_procedures(child_index)
                    if (.not. child_is_main_candidate) then
                        if (.not. has_procs) then
                            call append_program_statements(child_index)
                        end if
                    else
                        cycle
                    end if
                type is (function_def_node)
                    proc_indices = [proc_indices, child_index]
                type is (subroutine_def_node)
                    proc_indices = [proc_indices, child_index]
                class default
                    ! Non-procedure, non-program statements (bare statements)
                    if (is_host_level_statement(arena, child_index)) then
                        main_stmts = [main_stmts, child_index]
                    end if
                end select
            end do

            if (main_prog_index == 0 .and. candidate_prog_index > 0) then
                main_prog_index = candidate_prog_index
            end if
        class default
            return
        end select

        ! If we have procedures but no program node, create one from bare statements
        if (main_prog_index == 0 .and. size(proc_indices) > 0) then
            if (size(main_stmts) == 0) return
            ! Create program structure with bare statements + contains + procedures
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=0)

            body_size = 1 + size(main_stmts) + 1 + size(proc_indices)
            allocate (new_body(body_size))
            new_body(1) = implicit_none_index
            do i = 1, size(main_stmts)
                new_body(1 + i) = main_stmts(i)
            end do

            ! Add contains statement
            block
                type(contains_node) :: contains_stmt
                contains_stmt%line = 1
                contains_stmt%column = 1
                call arena%push(contains_stmt, "contains", 0)
                contains_index = arena%size
            end block
            new_body(1 + size(main_stmts) + 1) = contains_index

            ! Add procedures
            do i = 1, size(proc_indices)
                new_body(1 + size(main_stmts) + 1 + i) = proc_indices(i)
            end do

            ! Create program node
            block
                type(program_node) :: prog
                prog%name = "main"
                prog%body_indices = new_body
                prog%line = 1
                prog%column = 1
                call arena%push(prog, "program", 0)
                main_prog_index = arena%size
            end block

            ! Update parent indices
            arena%entries(implicit_none_index)%parent_index = main_prog_index
            do i = 1, size(main_stmts)
                arena%entries(main_stmts(i))%parent_index = main_prog_index
            end do
            arena%entries(contains_index)%parent_index = main_prog_index
            do i = 1, size(proc_indices)
                arena%entries(proc_indices(i))%parent_index = main_prog_index
            end do

            ! Replace the __MULTI_UNIT__ children with just the new program
            select type (root_prog => arena%entries(root_index)%node)
            type is (program_node)
                deallocate (root_prog%body_indices)
                allocate (root_prog%body_indices(1))
                root_prog%body_indices(1) = main_prog_index
                arena%entries(main_prog_index)%parent_index = root_index
            end select

            return
        end if

        if (main_prog_index == 0) return
        if (size(proc_indices) == 0) return

        select type (main_prog => arena%entries(main_prog_index)%node)
        type is (program_node)
            if (.not. allocated(main_prog%body_indices)) then
                allocate (main_prog%body_indices(0))
            end if

            has_contains = .false.
            do i = 1, size(main_prog%body_indices)
                idx = main_prog%body_indices(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (body_node => arena%entries(idx)%node)
                type is (contains_node)
                    has_contains = .true.
                end select
            end do

            if (.not. has_contains) then
                call insert_contains_statement(arena, main_prog, main_prog_index, &
                                               size(main_prog%body_indices) + 1)
            end if

            ! Merge: existing program body + bare statements + procedures
            if (allocated(main_prog%body_indices)) then
                allocate (filtered_body(0))
                do i = 1, size(main_prog%body_indices)
                    idx = main_prog%body_indices(i)
                    if (idx <= 0 .or. idx > arena%size) cycle
                    if (.not. allocated(arena%entries(idx)%node)) cycle
                    select type (body_node => arena%entries(idx)%node)
                    type is (function_def_node)
                        cycle
                    type is (subroutine_def_node)
                        cycle
                    class default
                        filtered_body = [filtered_body, idx]
                    end select
                end do
            else
                allocate (filtered_body(0))
            end if

            body_size = size(filtered_body)
            n_proc = size(proc_indices)
            allocate (new_body(body_size + size(main_stmts) + n_proc))
            pos = 0
            contains_pos = 0
            do i = 1, body_size
                idx = filtered_body(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (body_node => arena%entries(idx)%node)
                type is (contains_node)
                    contains_pos = i
                    exit
                end select
            end do
            if (contains_pos > 0) then
                if (contains_pos > 1) then
                    new_body(1:contains_pos - 1) = filtered_body(1:contains_pos - 1)
                    pos = contains_pos - 1
                end if
                if (size(main_stmts) > 0) then
                    new_body(pos + 1:pos + size(main_stmts)) = main_stmts
                    pos = pos + size(main_stmts)
                end if
                new_body(pos + 1) = filtered_body(contains_pos)
                pos = pos + 1
                if (contains_pos < body_size) then
                    new_body(pos + 1:pos + (body_size - contains_pos)) = &
                        filtered_body(contains_pos + 1:body_size)
                    pos = pos + (body_size - contains_pos)
                end if
            else
                if (body_size > 0) then
                    new_body(1:body_size) = filtered_body
                    pos = body_size
                end if
                if (size(main_stmts) > 0) then
                    new_body(pos + 1:pos + size(main_stmts)) = main_stmts
                    pos = pos + size(main_stmts)
                end if
            end if
            if (n_proc > 0) then
                new_body(pos + 1:pos + n_proc) = proc_indices
            end if
            main_prog%body_indices = new_body

            ! Update parent indices for the newly added bare statements
            do i = 1, size(main_stmts)
                arena%entries(main_stmts(i))%parent_index = main_prog_index
            end do
        end select

        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = main_prog_index
        end do

        root_index = main_prog_index

    contains

        subroutine collect_program_procedures(program_idx)
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        proc_indices = [proc_indices, stmt_idx]
                    type is (subroutine_def_node)
                        proc_indices = [proc_indices, stmt_idx]
                    end select
                end do
            end select
        end subroutine collect_program_procedures

        subroutine append_program_statements(program_idx)
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        cycle
                    type is (subroutine_def_node)
                        cycle
                    type is (implicit_statement_node)
                        cycle
                    type is (contains_node)
                        cycle
                    type is (end_statement_node)
                        cycle
                    class default
                        if (is_host_level_statement(arena, stmt_idx)) then
                            main_stmts = [main_stmts, stmt_idx]
                        end if
                    end select
                end do
            end select
        end subroutine append_program_statements

        logical function program_has_executable_statements(arena, program_idx) &
            result(has_exec)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            has_exec = .false.
            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        cycle
                    type is (subroutine_def_node)
                        cycle
                    type is (implicit_statement_node)
                        cycle
                    type is (contains_node)
                        exit
                    type is (end_statement_node)
                        cycle
                    type is (comment_node)
                        cycle
                    type is (directive_node)
                        cycle
                    type is (blank_line_node)
                        cycle
                    type is (declaration_node)
                        cycle
                    class default
                        has_exec = .true.
                        return
                    end select
                end do
            end select
        end function program_has_executable_statements

        logical function program_contains_procedures(arena, program_idx) &
            result(has_procs)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            has_procs = .false.
            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        has_procs = .true.
                        return
                    type is (subroutine_def_node)
                        has_procs = .true.
                        return
                    end select
                end do
            end select
        end function program_contains_procedures

        logical function is_host_level_statement(arena, node_idx) result(is_host)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_idx

            is_host = .false.
            if (node_idx <= 0 .or. node_idx > arena%size) return
            if (.not. allocated(arena%entries(node_idx)%node)) return

            select type (stmt => arena%entries(node_idx)%node)
            type is (assignment_node)
                is_host = .true.
            type is (print_statement_node)
                is_host = .true.
            type is (if_node)
                is_host = .true.
            type is (do_loop_node)
                is_host = .true.
            type is (subroutine_call_node)
                is_host = .true.
            end select
        end function is_host_level_statement
    end subroutine promote_functions_to_internal_program

    ! Check if AST already contains a module node
    function has_existing_module_in_ast(arena) result(has_module)
        type(ast_arena_t), intent(in) :: arena
        logical :: has_module
        integer :: i

        has_module = .false.

        ! Scan all nodes in arena for a module node
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                has_module = .true.
                exit
            end select
        end do
    end function has_existing_module_in_ast

    logical function requires_lazy_internalization(arena, prog_index) &
        result(needs_wrapping)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer :: i, idx
        character(len=64), allocatable :: host_names(:)
        character(len=64), allocatable :: proc_names(:)

        needs_wrapping = .false.
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (root => arena%entries(prog_index)%node)
        type is (program_node)
            if (trim(root%name) /= "__MULTI_UNIT__") return
            if (.not. allocated(root%body_indices)) return

            needs_wrapping = .true.
            call collect_host_assignment_names(arena, root, host_names)
            do i = 1, size(root%body_indices)
                idx = root%body_indices(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (child => arena%entries(idx)%node)
                type is (program_node)
                    if (.not. is_implicit_program_name(child%name)) then
                        needs_wrapping = .false.
                        return
                    end if
                type is (function_def_node)
                    call collect_procedure_assignment_names(arena, idx, proc_names)
                    if (has_name_intersection(host_names, proc_names)) return
                type is (subroutine_def_node)
                    call collect_procedure_assignment_names(arena, idx, proc_names)
                    if (has_name_intersection(host_names, proc_names)) return
                class default
                    needs_wrapping = .false.
                    return
                end select
            end do
            needs_wrapping = .false.
        class default
            needs_wrapping = .false.
        end select
    end function requires_lazy_internalization

    logical function is_implicit_program_name(name) result(is_implicit)
        character(len=*), intent(in) :: name

        select case (trim(name))
        case ("main", "__IMPLICIT_MAIN__")
            is_implicit = .true.
        case default
            is_implicit = .false.
        end select
    end function is_implicit_program_name

    subroutine collect_host_assignment_names(arena, root_prog, host_names)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(in) :: root_prog
        character(len=64), allocatable, intent(inout) :: host_names(:)
        integer :: i, child_idx

        if (.not. allocated(root_prog%body_indices)) return

        do i = 1, size(root_prog%body_indices)
            child_idx = root_prog%body_indices(i)
            if (child_idx <= 0 .or. child_idx > arena%size) cycle
            if (.not. allocated(arena%entries(child_idx)%node)) cycle
            select type (child => arena%entries(child_idx)%node)
            type is (program_node)
                if (is_implicit_program_name(child%name)) then
                    call collect_program_assignment_names(arena, child_idx, &
                                                          host_names)
                end if
            end select
        end do
    end subroutine collect_host_assignment_names

    subroutine collect_program_assignment_names(arena, prog_idx, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_idx
        character(len=64), allocatable, intent(inout) :: names(:)
        integer :: i, stmt_idx

        if (prog_idx <= 0 .or. prog_idx > arena%size) return
        if (.not. allocated(arena%entries(prog_idx)%node)) return
        select type (prog => arena%entries(prog_idx)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                stmt_idx = prog%body_indices(i)
                call collect_assignment_from_node(arena, stmt_idx, names, &
                                                  skip_procedures=.true.)
            end do
        end select
    end subroutine collect_program_assignment_names

    subroutine collect_procedure_assignment_names(arena, proc_idx, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        character(len=64), allocatable, intent(inout) :: names(:)
        integer :: i, stmt_idx

        if (proc_idx <= 0 .or. proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        select type (proc => arena%entries(proc_idx)%node)
        type is (function_def_node)
            if (.not. allocated(proc%body_indices)) return
            do i = 1, size(proc%body_indices)
                stmt_idx = proc%body_indices(i)
                call collect_assignment_from_node(arena, stmt_idx, names, &
                                                  skip_procedures=.false.)
            end do
        type is (subroutine_def_node)
            if (.not. allocated(proc%body_indices)) return
            do i = 1, size(proc%body_indices)
                stmt_idx = proc%body_indices(i)
                call collect_assignment_from_node(arena, stmt_idx, names, &
                                                  skip_procedures=.false.)
            end do
        end select
    end subroutine collect_procedure_assignment_names

    recursive subroutine collect_assignment_from_node(arena, node_index, names, &
                                                      skip_procedures)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=64), allocatable, intent(inout) :: names(:)
        logical, intent(in) :: skip_procedures
        integer :: child_i, child_target

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            call record_identifier_name(arena, node%target_index, names)
            return
        type is (function_def_node)
            if (skip_procedures) return
        type is (subroutine_def_node)
            if (skip_procedures) return
        type is (program_node)
            if (.not. allocated(node%body_indices)) return
            do child_i = 1, size(node%body_indices)
                child_target = node%body_indices(child_i)
                call collect_assignment_from_node(arena, child_target, names, &
                     skip_procedures)
            end do
            return
        end select

        if (allocated(arena%entries(node_index)%child_indices)) then
            do child_i = 1, size(arena%entries(node_index)%child_indices)
                child_target = arena%entries(node_index)%child_indices(child_i)
                call collect_assignment_from_node(arena, child_target, names, &
                     skip_procedures)
            end do
        end if
    end subroutine collect_assignment_from_node

    recursive subroutine record_identifier_name(arena, node_index, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=64), allocatable, intent(inout) :: names(:)

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (id => arena%entries(node_index)%node)
        type is (identifier_node)
            call append_unique_name(names, trim(to_lower(id%name)))
        type is (call_or_subscript_node)
            if (id%base_expr_index > 0) then
                call record_identifier_name(arena, id%base_expr_index, names)
            end if
        type is (component_access_node)
            if (id%base_expr_index > 0) then
                call record_identifier_name(arena, id%base_expr_index, names)
            end if
        end select
    end subroutine record_identifier_name

    subroutine append_unique_name(names, candidate)
        character(len=64), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: candidate
        character(len=64) :: lowered
        integer :: n

        lowered = adjustl(candidate)
        if (len_trim(lowered) == 0) return

        lowered = to_lower(lowered)

        if (.not. allocated(names)) then
            allocate (names(1))
            names(1) = lowered
            return
        end if

        do n = 1, size(names)
            if (names(n) == lowered) return
        end do

        names = [names, lowered]
    end subroutine append_unique_name

    logical function has_name_intersection(left, right) result(has_common)
        character(len=64), allocatable, intent(in) :: left(:)
        character(len=64), allocatable, intent(in) :: right(:)
        integer :: i, j

        has_common = .false.
        if (.not. allocated(left)) return
        if (.not. allocated(right)) return

        do i = 1, size(left)
            do j = 1, size(right)
                if (left(i) == right(j)) then
                    has_common = .true.
                    return
                end if
            end do
        end do
    end function has_name_intersection

end module frontend_transformation_analysis
