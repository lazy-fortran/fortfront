module frontend_program_builders
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, assignment_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_control, only: if_node, do_loop_node
    use ast_nodes_procedure, only: subroutine_call_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_misc, only: implicit_statement_node, contains_node, &
                              end_statement_node, comment_node, directive_node, &
                              blank_line_node
    use ast_factory, only: push_implicit_statement
    use standardizer_program, only: insert_contains_statement
    use procedure_classification, only: procedure_has_entry_statement
    implicit none
    private

    public :: collect_program_procedures
    public :: append_program_statements
    public :: program_has_executable_statements
    public :: program_contains_procedures
    public :: is_host_level_statement
    public :: handle_mixed_construct_container
    public :: scan_multi_unit_program
    public :: create_program_from_bare_statements
    public :: merge_procedures_into_program
    public :: filter_procs_with_entry

contains

    subroutine handle_mixed_construct_container(arena, root_index, root, &
                                                proc_indices, main_stmts)
        use ast_nodes_data, only: mixed_construct_container_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        class(*), intent(in) :: root
        integer, allocatable, intent(inout) :: proc_indices(:)
        integer, allocatable, intent(inout) :: main_stmts(:)
        integer :: i, child_index, implicit_none_index, contains_index, prog_index
        integer :: body_size
        integer, allocatable :: new_body(:)

        select type (mixed_root => root)
        type is (mixed_construct_container_node)
            if (allocated(mixed_root%implicit_declaration_indices)) then
                do i = 1, size(mixed_root%implicit_declaration_indices)
                    child_index = mixed_root%implicit_declaration_indices(i)
                    if (is_host_level_statement(arena, child_index)) then
                        main_stmts = [main_stmts, child_index]
                    end if
                end do
            end if
            if (allocated(mixed_root%explicit_program_indices)) then
                do i = 1, size(mixed_root%explicit_program_indices)
                    child_index = mixed_root%explicit_program_indices(i)
                    if (.not. arena%has_node_at(child_index)) cycle
                    select type (child => arena%entries(child_index)%node)
                    type is (function_def_node)
                        proc_indices = [proc_indices, child_index]
                    type is (subroutine_def_node)
                        proc_indices = [proc_indices, child_index]
                    end select
                end do
            end if

            ! Filter out procedures with ENTRY statements - they cannot be
            ! contained in internal program procedures.
            call filter_procs_with_entry(arena, proc_indices)

            if (size(proc_indices) > 0) then
                if (size(main_stmts) == 0) return

                implicit_none_index = push_implicit_statement(arena, .true., &
                                                              line=1, column=1, &
                                                              parent_index=0)

                body_size = 1 + size(main_stmts) + 1 + size(proc_indices)
                allocate (new_body(body_size))
                new_body(1) = implicit_none_index
                do i = 1, size(main_stmts)
                    new_body(1 + i) = main_stmts(i)
                end do

                block
                    type(contains_node) :: contains_stmt
                    contains_stmt%line = 1
                    contains_stmt%column = 1
                    call arena%push(contains_stmt, "contains", 0)
                    contains_index = arena%size
                end block
                new_body(1 + size(main_stmts) + 1) = contains_index

                do i = 1, size(proc_indices)
                    new_body(1 + size(main_stmts) + 1 + i) = proc_indices(i)
                end do

                block
                    type(program_node) :: prog
                    prog%name = "main"
                    prog%body_indices = new_body
                    prog%line = 1
                    prog%column = 1
                    call arena%push(prog, "program", 0)
                    prog_index = arena%size
                end block

                arena%entries(implicit_none_index)%parent_index = prog_index
                do i = 1, size(main_stmts)
                    arena%entries(main_stmts(i))%parent_index = prog_index
                end do
                arena%entries(contains_index)%parent_index = prog_index
                do i = 1, size(proc_indices)
                    arena%entries(proc_indices(i))%parent_index = prog_index
                end do

                root_index = prog_index
            end if
        end select
    end subroutine handle_mixed_construct_container

    subroutine scan_multi_unit_program(arena, root, main_prog_index, &
                                       candidate_prog_index, proc_indices, main_stmts)
        type(ast_arena_t), intent(inout) :: arena
        class(*), intent(in) :: root
        integer, intent(inout) :: main_prog_index
        integer, intent(inout) :: candidate_prog_index
        integer, allocatable, intent(inout) :: proc_indices(:)
        integer, allocatable, intent(inout) :: main_stmts(:)
        integer :: i, child_index
        logical :: child_is_main_candidate, has_exec, has_procs

        select type (prog_root => root)
        type is (program_node)
            if (trim(prog_root%name) /= "__MULTI_UNIT__") return
            if (.not. allocated(prog_root%body_indices)) return

            do i = 1, size(prog_root%body_indices)
                child_index = prog_root%body_indices(i)
                if (.not. arena%has_node_at(child_index)) cycle

                select type (child => arena%entries(child_index)%node)
                type is (program_node)
                    child_is_main_candidate = .false.
                    has_exec = program_has_executable_statements(arena, child_index)
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
                    call collect_program_procedures(arena, child_index, proc_indices)
                    if (.not. child_is_main_candidate) then
                        if (.not. has_procs) then
                            call append_program_statements(arena, child_index, &
                                                           main_stmts)
                        end if
                    else
                        cycle
                    end if
                type is (function_def_node)
                    proc_indices = [proc_indices, child_index]
                type is (subroutine_def_node)
                    proc_indices = [proc_indices, child_index]
                class default
                    if (is_host_level_statement(arena, child_index)) then
                        main_stmts = [main_stmts, child_index]
                    end if
                end select
            end do

            if (main_prog_index == 0 .and. candidate_prog_index > 0) then
                main_prog_index = candidate_prog_index
            end if
        end select
    end subroutine scan_multi_unit_program

    subroutine create_program_from_bare_statements(arena, root_index, &
                                                   main_prog_index, proc_indices, &
                                                   main_stmts)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer, intent(inout) :: main_prog_index
        integer, allocatable, intent(in) :: proc_indices(:)
        integer, allocatable, intent(in) :: main_stmts(:)
        integer :: i, implicit_none_index, contains_index, body_size
        integer, allocatable :: new_body(:)

        if (main_prog_index /= 0 .or. size(proc_indices) == 0) return
        if (size(main_stmts) == 0) return

        implicit_none_index = push_implicit_statement(arena, .true., &
                                                      line=1, column=1, parent_index=0)

        body_size = 1 + size(main_stmts) + 1 + size(proc_indices)
        allocate (new_body(body_size))
        new_body(1) = implicit_none_index
        do i = 1, size(main_stmts)
            new_body(1 + i) = main_stmts(i)
        end do

        block
            type(contains_node) :: contains_stmt
            contains_stmt%line = 1
            contains_stmt%column = 1
            call arena%push(contains_stmt, "contains", 0)
            contains_index = arena%size
        end block
        new_body(1 + size(main_stmts) + 1) = contains_index

        do i = 1, size(proc_indices)
            new_body(1 + size(main_stmts) + 1 + i) = proc_indices(i)
        end do

        block
            type(program_node) :: prog
            prog%name = "main"
            prog%body_indices = new_body
            prog%line = 1
            prog%column = 1
            call arena%push(prog, "program", 0)
            main_prog_index = arena%size
        end block

        arena%entries(implicit_none_index)%parent_index = main_prog_index
        do i = 1, size(main_stmts)
            arena%entries(main_stmts(i))%parent_index = main_prog_index
        end do
        arena%entries(contains_index)%parent_index = main_prog_index
        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = main_prog_index
        end do

        select type (root_prog => arena%entries(root_index)%node)
        type is (program_node)
            deallocate (root_prog%body_indices)
            allocate (root_prog%body_indices(1))
            root_prog%body_indices(1) = main_prog_index
            arena%entries(main_prog_index)%parent_index = root_index
        end select
    end subroutine create_program_from_bare_statements

    subroutine merge_procedures_into_program(arena, main_prog_index, &
                                             proc_indices, main_stmts)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: main_prog_index
        integer, allocatable, intent(in) :: proc_indices(:)
        integer, allocatable, intent(in) :: main_stmts(:)
        integer :: i, idx, body_size, n_proc, contains_pos, pos
        integer, allocatable :: new_body(:), filtered_body(:)
        logical :: has_contains

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
                if (.not. arena%has_node_at(idx)) cycle
                select type (body_node => arena%entries(idx)%node)
                type is (contains_node)
                    has_contains = .true.
                end select
            end do

            if (.not. has_contains) then
                call insert_contains_statement(arena, main_prog, main_prog_index, &
                                               size(main_prog%body_indices) + 1)
            end if

            if (allocated(main_prog%body_indices)) then
                allocate (filtered_body(0))
                do i = 1, size(main_prog%body_indices)
                    idx = main_prog%body_indices(i)
                    if (.not. arena%has_node_at(idx)) cycle
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
                if (.not. arena%has_node_at(idx)) cycle
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

            do i = 1, size(main_stmts)
                arena%entries(main_stmts(i))%parent_index = main_prog_index
            end do
        end select

        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = main_prog_index
        end do
    end subroutine merge_procedures_into_program

    subroutine collect_program_procedures(arena, program_idx, proc_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: program_idx
        integer, allocatable, intent(inout) :: proc_indices(:)
        integer :: j, stmt_idx

        if (.not. arena%has_node_at(program_idx)) return

        select type (prog => arena%entries(program_idx)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                stmt_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(stmt_idx)) cycle
                select type (stmt => arena%entries(stmt_idx)%node)
                type is (function_def_node)
                    proc_indices = [proc_indices, stmt_idx]
                type is (subroutine_def_node)
                    proc_indices = [proc_indices, stmt_idx]
                end select
            end do
        end select
    end subroutine collect_program_procedures

    subroutine append_program_statements(arena, program_idx, main_stmts)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: program_idx
        integer, allocatable, intent(inout) :: main_stmts(:)
        integer :: j, stmt_idx

        if (.not. arena%has_node_at(program_idx)) return

        select type (prog => arena%entries(program_idx)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                stmt_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(stmt_idx)) cycle
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
        if (.not. arena%has_node_at(program_idx)) return

        select type (prog => arena%entries(program_idx)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                stmt_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(stmt_idx)) cycle
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
        if (.not. arena%has_node_at(program_idx)) return

        select type (prog => arena%entries(program_idx)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                stmt_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(stmt_idx)) cycle
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
        if (.not. arena%has_node_at(node_idx)) return

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

    subroutine filter_procs_with_entry(arena, proc_indices)
        ! Remove procedures containing ENTRY statements from the list.
        ! ENTRY statements cannot appear in contained (internal) procedures per
        ! ISO/IEC 1539-1:2018 Section 15.6.2.6.
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(inout) :: proc_indices(:)
        integer, allocatable :: filtered(:)
        integer :: i

        if (.not. allocated(proc_indices)) return
        if (size(proc_indices) == 0) return

        allocate (filtered(0))
        do i = 1, size(proc_indices)
            if (.not. procedure_has_entry_statement(arena, proc_indices(i))) then
                filtered = [filtered, proc_indices(i)]
            end if
        end do
        proc_indices = filtered
    end subroutine filter_procs_with_entry

end module frontend_program_builders
