    subroutine collect_procedures_and_target(arena, root_prog, all_procedures, &
                                             target_prog_idx)
        type(ast_arena_t), intent(in) :: arena
        class(multi_unit_container_node), intent(in) :: root_prog
        integer, allocatable, intent(out) :: all_procedures(:)
        integer, intent(out) :: target_prog_idx
        integer :: i
        integer :: first_main_idx

        allocate (all_procedures(0))
        target_prog_idx = 0
        first_main_idx = 0

        if (.not. allocated(root_prog%body_indices)) return

        do i = 1, size(root_prog%body_indices)
            if (root_prog%body_indices(i) <= 0 .or. &
                root_prog%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(root_prog%body_indices(i))%node)) cycle

            select type (child => arena%entries(root_prog%body_indices(i))%node)
            type is (program_node)
                if (trim(child%name) /= "" .and. child%name /= "main" .and. &
                    child%name /= "MAIN") then
                    target_prog_idx = root_prog%body_indices(i)
                else if (first_main_idx == 0) then
                    first_main_idx = root_prog%body_indices(i)
                end if
            type is (function_def_node)
                all_procedures = [all_procedures, root_prog%body_indices(i)]
            type is (subroutine_def_node)
                all_procedures = [all_procedures, root_prog%body_indices(i)]
            end select
        end do

        if (target_prog_idx == 0) target_prog_idx = first_main_idx
    end subroutine collect_procedures_and_target

    subroutine filter_hoistable_procedures(arena, all_procedures, target_prog_idx, &
                                           procedures)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: all_procedures(:)
        integer, intent(in) :: target_prog_idx
        integer, allocatable, intent(out) :: procedures(:)
        integer :: i

        allocate (procedures(0))
        do i = 1, size(all_procedures)
            if (should_hoist_procedure(arena, all_procedures(i), target_prog_idx)) then
                procedures = [procedures, all_procedures(i)]
            end if
        end do
    end subroutine filter_hoistable_procedures

    subroutine filter_out_entry_procedures(arena, proc_indices)
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
    end subroutine filter_out_entry_procedures

    subroutine remove_procedures_from_body(root_prog, procedures)
        class(multi_unit_container_node), intent(inout) :: root_prog
        integer, intent(in) :: procedures(:)
        integer, allocatable :: new_body(:)
        integer :: i

        allocate (new_body(0))
        if (allocated(root_prog%body_indices)) then
            do i = 1, size(root_prog%body_indices)
                if (any(root_prog%body_indices(i) == procedures)) cycle
                new_body = [new_body, root_prog%body_indices(i)]
            end do
        end if
        root_prog%body_indices = new_body
    end subroutine remove_procedures_from_body

    function find_contains_position(arena, target) result(contains_pos)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(in) :: target
        integer :: contains_pos
        integer :: i

        contains_pos = 0
        if (.not. allocated(target%body_indices)) return

        do i = 1, size(target%body_indices)
            if (target%body_indices(i) <= 0 .or. &
                target%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(target%body_indices(i))%node)) cycle

            select type (stmt => arena%entries(target%body_indices(i))%node)
            type is (contains_node)
                contains_pos = i
                exit
            end select
        end do
    end function find_contains_position

    subroutine ensure_contains_exists(arena, target, target_prog_idx, contains_pos)
        type(ast_arena_t), intent(inout) :: arena
        class(program_node), intent(inout) :: target
        integer, intent(in) :: target_prog_idx
        integer, intent(inout) :: contains_pos
        type(contains_node) :: contains_stmt
        integer :: contains_idx

        if (contains_pos > 0) return

        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", target_prog_idx)
        contains_idx = arena%size

        if (.not. allocated(target%body_indices)) then
            allocate (target%body_indices(1))
            target%body_indices(1) = contains_idx
        else
            target%body_indices = [target%body_indices, contains_idx]
        end if
        contains_pos = size(target%body_indices)
    end subroutine ensure_contains_exists

    subroutine insert_procedures_after_contains(target, procedures, contains_pos)
        class(program_node), intent(inout) :: target
        integer, intent(in) :: procedures(:)
        integer, intent(in) :: contains_pos
        integer, allocatable :: original(:)
        integer :: orig_size, insert_size

        if (allocated(target%body_indices)) then
            original = target%body_indices
            deallocate (target%body_indices)
        else
            allocate (original(0))
        end if

        orig_size = size(original)
        insert_size = size(procedures)
        allocate (target%body_indices(orig_size + insert_size))

        if (contains_pos >= 1) then
            target%body_indices(1:contains_pos) = original(1:contains_pos)
        end if
        target%body_indices(contains_pos + 1:contains_pos + insert_size) = procedures
        if (contains_pos < orig_size) then
            target%body_indices(contains_pos + insert_size + 1:) = &
                original(contains_pos + 1:orig_size)
        end if
    end subroutine insert_procedures_after_contains

    subroutine clean_external_declarations(arena, target, procedures)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(inout) :: target
        integer, intent(in) :: procedures(:)
        integer :: i, j
        integer, allocatable :: compressed(:)
        logical :: declares_function

        if (.not. allocated(target%body_indices)) return

        do i = 1, size(target%body_indices)
            if (target%body_indices(i) <= 0 .or. &
                target%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(target%body_indices(i))%node)) cycle

            select type (stmt => arena%entries(target%body_indices(i))%node)
            type is (declaration_node)
                declares_function = .false.
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do j = 1, size(stmt%var_names)
                        if (is_procedure_name(trim(stmt%var_names(j)), arena, &
                                              procedures)) then
                            declares_function = .true.
                            exit
                        end if
                    end do
                else
                    if (is_procedure_name(trim(stmt%var_name), arena, procedures)) then
                        declares_function = .true.
                    end if
                end if
                if (declares_function) target%body_indices(i) = 0
            end select
        end do

        allocate (compressed(0))
        do i = 1, size(target%body_indices)
            if (target%body_indices(i) /= 0) then
                compressed = [compressed, target%body_indices(i)]
            end if
        end do
        target%body_indices = compressed
    end subroutine clean_external_declarations

    subroutine merge_additional_main_programs(arena, root_prog, target_prog_idx)
        type(ast_arena_t), intent(inout) :: arena
        class(multi_unit_container_node), intent(inout) :: root_prog
        integer, intent(in) :: target_prog_idx
        integer, allocatable :: new_body(:)
        integer :: i, child_idx
        class(program_node), pointer :: target_prog => null()

        if (.not. allocated(root_prog%body_indices)) return
        if (.not. arena%has_node_at(target_prog_idx)) return
        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            target_prog => target
        class default
            return
        end select
        if (.not. associated(target_prog)) return

        allocate (new_body(0))
        do i = 1, size(root_prog%body_indices)
            child_idx = root_prog%body_indices(i)
            if (child_idx == target_prog_idx) then
                new_body = [new_body, child_idx]
                cycle
            end if
            if (child_idx <= 0 .or. child_idx > arena%size) then
                new_body = [new_body, child_idx]
                cycle
            end if
            if (.not. allocated(arena%entries(child_idx)%node)) then
                new_body = [new_body, child_idx]
                cycle
            end if
            select type (child => arena%entries(child_idx)%node)
            type is (program_node)
                if (trim(child%name) == "main" .or. &
                    trim(child%name) == "__IMPLICIT_MAIN__") then
                    call append_program_body_to_target(arena, target_prog_idx, &
                                                       child_idx)
                    cycle
                end if
            end select
            new_body = [new_body, child_idx]
        end do
        root_prog%body_indices = new_body
    end subroutine merge_additional_main_programs

    subroutine append_program_body_to_target(arena, target_prog_idx, &
                                             source_prog_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_prog_idx
        integer, intent(in) :: source_prog_idx
        integer :: i, child_idx

        if (.not. arena%has_node_at(source_prog_idx)) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return

        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            select type (source => arena%entries(source_prog_idx)%node)
            type is (program_node)
                if (.not. allocated(source%body_indices)) return
                if (.not. allocated(target%body_indices)) then
                    target%body_indices = source%body_indices
                else
                    target%body_indices = [target%body_indices, &
                                           source%body_indices]
                end if
                do i = 1, size(source%body_indices)
                    child_idx = source%body_indices(i)
                    if (child_idx <= 0 .or. child_idx > arena%size) cycle
                    arena%entries(child_idx)%parent_index = target_prog_idx
                end do
                deallocate (source%body_indices)
            end select
        end select
    end subroutine append_program_body_to_target

    function collect_target_procedures(arena, target_prog_idx) result(proc_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_prog_idx
        integer, allocatable :: proc_indices(:)
        integer :: i, child_idx

        allocate (proc_indices(0))
        if (.not. arena%has_node_at(target_prog_idx)) return

        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            if (.not. allocated(target%body_indices)) return
            do i = 1, size(target%body_indices)
                child_idx = target%body_indices(i)
                if (.not. arena%has_node_at(child_idx)) cycle
                select type (child => arena%entries(child_idx)%node)
                type is (function_def_node)
                    proc_indices = [proc_indices, child_idx]
                type is (subroutine_def_node)
                    proc_indices = [proc_indices, child_idx]
                end select
            end do
        end select
    end function collect_target_procedures

    subroutine remove_target_procedures_from_body(arena, target_prog_idx, &
                                                  procedures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_prog_idx
        integer, intent(in) :: procedures(:)
        integer, allocatable :: new_body(:)
        integer :: i

        if (size(procedures) == 0) return
        if (.not. arena%has_node_at(target_prog_idx)) return

        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            if (.not. allocated(target%body_indices)) return
            allocate (new_body(0))
            do i = 1, size(target%body_indices)
                if (any(target%body_indices(i) == procedures)) cycle
                new_body = [new_body, target%body_indices(i)]
            end do
            target%body_indices = new_body
        end select
    end subroutine remove_target_procedures_from_body

    subroutine normalize_multi_unit_container(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer :: i, target_prog_idx, contains_pos
        integer, allocatable :: procedures(:), all_procedures(:)
        integer, allocatable :: embedded_procs(:), lifted_procs(:)
        class(multi_unit_container_node), pointer :: root_prog => null()
        integer :: first_main_idx

        if (.not. arena%has_node_at(root_index)) return

        select type (root => arena%entries(root_index)%node)
        type is (multi_unit_container_node)
            root_prog => root
        class default
            return
        end select
        if (.not. associated(root_prog)) return

        call collect_procedures_and_target(arena, root_prog, all_procedures, &
                                           target_prog_idx)

        if (target_prog_idx == 0) return
        if (size(all_procedures) == 0) return

        if (.not. arena%has_node_at(target_prog_idx)) return
        select type (target_prog => arena%entries(target_prog_idx)%node)
        type is (program_node)
            ! Accept explicit program names as valid hoist targets
        class default
            return
        end select

        call merge_additional_main_programs(arena, root_prog, target_prog_idx)

        embedded_procs = collect_target_procedures(arena, target_prog_idx)
        call filter_hoistable_procedures(arena, all_procedures, target_prog_idx, &
                                         lifted_procs)

        ! Filter out embedded procedures with ENTRY statements. ENTRY statements
        ! cannot appear in contained procedures per ISO/IEC 1539-1:2018 Section
        ! 15.6.2.6. Such procedures must remain external.
        call filter_out_entry_procedures(arena, embedded_procs)

        if (size(embedded_procs) == 0 .and. size(lifted_procs) == 0) return

        call remove_target_procedures_from_body(arena, target_prog_idx, &
                                                embedded_procs)
        call remove_procedures_from_body(root_prog, lifted_procs)

        procedures = embedded_procs
        if (size(lifted_procs) > 0) then
            procedures = [procedures, lifted_procs]
        end if

        if (.not. allocated(arena%entries(target_prog_idx)%node)) return
        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            contains_pos = find_contains_position(arena, target)
            call ensure_contains_exists(arena, target, target_prog_idx, contains_pos)
            call insert_procedures_after_contains(target, procedures, contains_pos)

            do i = 1, size(procedures)
                if (procedures(i) > 0 .and. procedures(i) <= arena%size) then
                    arena%entries(procedures(i))%parent_index = target_prog_idx
                end if
            end do

            call clean_external_declarations(arena, target, procedures)
        end select
    end subroutine normalize_multi_unit_container

    logical function is_procedure_name(name, arena, proc_indices) result(match)
        character(len=*), intent(in) :: name
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_indices(:)
        integer :: k

        match = .false.
        do k = 1, size(proc_indices)
            if (.not. arena%has_node_at(proc_indices(k))) cycle
            select type (proc_node => arena%entries(proc_indices(k))%node)
            type is (function_def_node)
                if (trim(proc_node%name) == trim(name)) then
                    match = .true.
                    return
                end if
            type is (subroutine_def_node)
                if (trim(proc_node%name) == trim(name)) then
                    match = .true.
                    return
                end if
            end select
        end do
    end function is_procedure_name

    ! Run code generation phase
    subroutine run_code_generation_phase(compiler_arena, prog_index, output)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: output

        call compiler_arena%next_phase("codegen")
        call maybe_dump_program_overview(compiler_arena%ast, prog_index)
        output = generate_code_from_arena(compiler_arena%ast, prog_index)
        output = add_line_continuations(output)
    end subroutine run_code_generation_phase

    subroutine maybe_dump_program_overview(arena, prog_index)
        use, intrinsic :: iso_fortran_env, only: error_unit
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=8) :: flag
        integer :: status
        integer :: i, j, child_idx, member_idx
        character(len=:), allocatable :: child_type

        call get_environment_variable('FORTFRONT_DEBUG_DUMP_AST', flag, status=status)
        if (status /= 0) return
        if (len_trim(flag) == 0) return

        write (error_unit, '(A)') 'DEBUG AST: program overview'
        write (error_unit, '(A,I0)') '  root index: ', prog_index
        do i = 1, min(arena%size, size(arena%entries))
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (program_node)
                if (allocated(node%body_indices)) then
                    write (error_unit, '(A,I0,2X,A,2X,I0)') &
                        '  program idx', i, trim(node%name), size(node%body_indices)
                    do j = 1, size(node%body_indices)
                        child_idx = node%body_indices(j)
                        if (child_idx <= 0) cycle
                        if (child_idx > arena%size) cycle
                        if (allocated(arena%entries(child_idx)%node_type)) then
                            child_type = trim(arena%entries(child_idx)%node_type)
                        else
                            child_type = '<unknown>'
                        end if
                        write (error_unit, '(A,1X,I0,2X,A)') '    body', &
                            child_idx, child_type
                    end do
                else
                    write (error_unit, '(A,I0,2X,A,2X,A)') &
                        '  program idx', i, trim(node%name), 'no body'
                end if
            type is (function_def_node)
                write (error_unit, '(A,I0,2X,A)') '  function idx', i, trim(node%name)
            type is (subroutine_def_node)
                write (error_unit, '(A,I0,2X,A)') '  subroutine idx', i, &
                    trim(node%name)
            type is (declaration_node)
                write (error_unit, '(A,I0,2X,A,2X,A)') '  decl idx', i, &
                    trim(node%type_name), trim(node%var_name)
            end select
        end do
    end subroutine maybe_dump_program_overview

    ! Save current configuration
    subroutine save_current_configuration(saved_size, saved_char, &
                                          saved_line_length, &
                                          saved_standardize_types, &
                                          saved_standardizer_types)
        integer, intent(out) :: saved_size, saved_line_length
        character(len=1), intent(out) :: saved_char
        logical, intent(out) :: saved_standardize_types, saved_standardizer_types

        call get_indent_config(saved_size, saved_char)
        call get_line_length_config(saved_line_length)
        call get_type_standardization(saved_standardize_types)
        call get_standardizer_type_standardization(saved_standardizer_types)
    end subroutine save_current_configuration

    ! Apply format options
    subroutine apply_format_options(format_opts)
        type(format_options_t), intent(in) :: format_opts

        call set_indent_config(format_opts%indent_size, format_opts%indent_char)
        call set_line_length_config(format_opts%line_length)
        call set_type_standardization(format_opts%standardize_types)
        call set_standardizer_type_standardization(format_opts%standardize_types)
    end subroutine apply_format_options

    ! Restore configuration
    subroutine restore_configuration(saved_size, saved_char, saved_line_length, &
                                     saved_standardize_types, saved_standardizer_types)
        integer, intent(in) :: saved_size, saved_line_length
        character(len=1), intent(in) :: saved_char
        logical, intent(in) :: saved_standardize_types, saved_standardizer_types

        call set_indent_config(saved_size, saved_char)
        call set_line_length_config(saved_line_length)
        call set_type_standardization(saved_standardize_types)
        call set_standardizer_type_standardization(saved_standardizer_types)
    end subroutine restore_configuration

    pure subroutine decode_bom_if_needed(raw_text, decoded)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded

        call decode_source_bom(raw_text, decoded)
    end subroutine decode_bom_if_needed

    ! Detect binary data early; allow UTF-8/high-bit text
    pure logical function contains_binary_data(text) result(has_binary)
        character(len=*), intent(in) :: text
        integer :: i, code, limit

        has_binary = .false.
        limit = min(len(text), 4096)
        if (limit <= 0) return

        do i = 1, limit
            code = iachar(text(i:i))
            if (code == 0) then
                has_binary = .true.
                return
            end if
            ! Reject control characters except common text file markers:
            ! TAB(9), LF(10), FF(12), CR(13), Ctrl-Z/SUB(26)
            if (code < 32 .and. code /= 9 .and. code /= 10 .and. &
                code /= 12 .and. code /= 13 .and. code /= 26) then
                has_binary = .true.
                return
            end if
            ! High-bit (>=128) allowed to support UTF-8 and extended text
        end do
    end function contains_binary_data

    ! Check if input contains only whitespace characters (spaces, tabs, newlines)
    function is_whitespace_only(input) result(is_whitespace)
        character(len=*), intent(in) :: input
        logical :: is_whitespace
        integer :: i

        is_whitespace = .true.
        do i = 1, len(input)
            if (input(i:i) /= ' ' .and. input(i:i) /= char(9) .and. &  ! space and tab
                input(i:i) /= new_line('A')) then  ! newline
                is_whitespace = .false.
                exit
            end if
        end do
    end function is_whitespace_only

    pure logical function has_leading_comment(src)
        character(len=*), intent(in) :: src
        integer :: n, k
        n = len(src)
        k = 1
        do while (k <= n)
            if (src(k:k) == ' ' .or. iachar(src(k:k)) == 9 .or. &
                src(k:k) == new_line('A')) then
                k = k + 1
            else
                exit
            end if
        end do
        has_leading_comment = (k <= n .and. src(k:k) == '!')
    end function has_leading_comment

    ! Extract a contiguous leading block of comment lines from the raw input.
    ! Leading comment lines start with optional whitespace followed by !.
    pure function extract_leading_comment_block(src) result(block_text)
        character(len=*), intent(in) :: src
        character(len=:), allocatable :: block_text
        integer :: n, i, j
        logical :: saw_comment

        block_text = ""
        saw_comment = .false.
        n = len(src)
        i = 1

        ! Quick check: if the first non-whitespace character is not !,
        ! there is no leading comment block to preserve.
        do while (i <= n)
            if (src(i:i) == ' ' .or. iachar(src(i:i)) == 9 .or. &
                src(i:i) == new_line('A')) then
                i = i + 1
            else
                exit
            end if
        end do
        if (i > n) then
            deallocate (block_text)
            return
        end if
        if (src(i:i) /= '!') then
            deallocate (block_text)
            return
        end if

        ! Reset to start scanning from the beginning to capture all leading comments
        i = 1

        do while (i <= n)
            ! Find start of line (skip nothing; i already at start)
            j = i
            ! Skip leading spaces/tabs
            do while (j <= n)
                if (src(j:j) == ' ' .or. iachar(src(j:j)) == 9) then
                    j = j + 1
                else
                    exit
                end if
            end do

            if (j > n) exit

            select case (src(j:j))
            case ('!')
                ! Comment line: collect until newline
                saw_comment = .true.
                if (len(block_text) > 0) block_text = block_text // new_line('A')
                do while (i <= n .and. src(i:i) /= new_line('A'))
                    block_text = block_text // src(i:i)
                    i = i + 1
                end do
                ! Trim trailing spaces from collected line
                block_text = trim(block_text)
                ! Consume newline if present
                if (i <= n .and. src(i:i) == new_line('A')) i = i + 1
            case (char(10))  ! newline encountered at line start
                if (saw_comment) then
                    if (len(block_text) > 0) block_text = block_text // new_line('A')
                end if
                i = i + 1
            case default
                exit  ! Non-comment, non-blank line: stop
            end select
        end do

        if (len(block_text) == 0) then
            deallocate (block_text)
        end if
    end function extract_leading_comment_block

    ! ========================================================================
    ! AST-BASED WRAPPING FUNCTIONS (Clean, proper compiler architecture)
    ! ========================================================================

    subroutine collect_procedure_indices(arena, proc_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(out) :: proc_indices(:)
        integer :: i

        allocate (proc_indices(0))
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                proc_indices = [proc_indices, i]
            type is (subroutine_def_node)
                proc_indices = [proc_indices, i]
            end select
        end do
    end subroutine collect_procedure_indices

    subroutine create_module_with_procedures(arena, context, proc_indices, mod_index)
        type(ast_arena_t), intent(inout) :: arena
        type(transform_context_t), intent(in) :: context
        integer, intent(in) :: proc_indices(:)
        integer, intent(out) :: mod_index
        type(module_node) :: mod
        character(len=:), allocatable :: original_module_name
        integer :: i

        ! Check if theres already a module in the AST and preserve its name
        original_module_name = ""
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                if (len(original_module_name) == 0) then
                    original_module_name = node%name
                    exit
                end if
            end select
        end do

        if (len(original_module_name) > 0) then
            mod%name = original_module_name
        else
            mod%name = context%module_name
        end if

        mod%has_contains = .true.
        mod%procedure_indices = proc_indices
        mod%line = 1
        mod%column = 1
        call arena%push(mod, "module", 0)
        mod_index = arena%size
    end subroutine create_module_with_procedures

! Wrap procedures in a module (AST manipulation, no strings!)
    subroutine wrap_ast_in_module_only(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(transform_context_t), intent(in) :: context
        type(module_node) :: mod
        integer :: i, mod_index
        integer, allocatable :: proc_indices(:)

        call collect_procedure_indices(arena, proc_indices)

        if (size(proc_indices) == 0) return

        call create_module_with_procedures(arena, context, proc_indices, mod_index)

        ! Update parent indices of procedures
        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = mod_index
        end do

        ! Set new root
        root_index = mod_index
    end subroutine wrap_ast_in_module_only

    ! Wrap procedures in module and main code in program (AST manipulation)
    subroutine wrap_ast_in_module_and_program(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(transform_context_t), intent(in) :: context
        type(module_node) :: mod
        type(program_node), pointer :: prog_ptr
        type(use_statement_node) :: use_stmt
        integer :: i, mod_index, use_index
        integer, allocatable :: proc_indices(:), new_body(:)

        call collect_procedure_indices(arena, proc_indices)

        if (size(proc_indices) == 0) return

        call create_module_with_procedures(arena, context, proc_indices, mod_index)

        ! Update procedure parent indices
        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = mod_index
        end do

        ! Remove procedures from program body and add use statement
        if (root_index > 0 .and. root_index <= arena%size) then
            if (allocated(arena%entries(root_index)%node)) then
                select type (prog => arena%entries(root_index)%node)
                type is (program_node)
                    ! Create use statement
                    use_stmt%module_name = context%module_name
                    use_stmt%line = 1
                    use_stmt%column = 1
                    call arena%push(use_stmt, "use", root_index)
                    use_index = arena%size

                    ! Remove procedure indices from program body
                    if (allocated(prog%body_indices)) then
                        allocate (new_body(0))
                        do i = 1, size(prog%body_indices)
                            if (.not. any(prog%body_indices(i) == proc_indices)) then
                                new_body = [new_body, prog%body_indices(i)]
                            end if
                        end do
                        ! Prepend use statement
                        prog%body_indices = [use_index, new_body]
                    end if
                end select
            end if
        end if

        ! Note: We dont change root_index because the program is still the root
        ! Code generation will emit both the module and the program
    end subroutine wrap_ast_in_module_and_program
