module frontend_mixed_constructs
    ! Mixed construct detection and handling functionality
    ! Supports Issue #511 - mixed module/program constructs

    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_COMMENT, TK_NEWLINE, &
        TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
        TK_UNKNOWN, TK_WHITESPACE
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: mixed_construct_container_node, &
        create_mixed_construct_container
    use mixed_construct_detector, only: &
        mixed_construct_result_t
    use error_reporting, only: error_collection_t

    implicit none
    private

    ! Public mixed constructs interface
    public :: parse_mixed_constructs, create_mixed_construct_container_arena
    public :: parse_declaration_range, parse_program_range

contains

    ! Parse mixed constructs (Issue #511 support)
    subroutine parse_mixed_constructs(tokens, arena, mixed_result, &
            prog_index, error_msg, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(mixed_construct_result_t), intent(in) :: mixed_result
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        integer, allocatable :: implicit_indices(:), explicit_indices(:)
        integer :: i, stmt_index, range_start, range_end
        type(token_t), allocatable, target :: range_tokens(:)
        character(len=:), allocatable :: module_name
        logical, allocatable :: used_tokens(:)
        integer :: num_tokens
        character(len=8) :: dump_flag
        integer :: env_status

        error_msg = ""
        allocate (implicit_indices(0))
        allocate (explicit_indices(0))

        ! Derive a stable, content-based module name so distinct sources do not
        ! collide if several mixed-construct containers ever share one arena.
        ! The same input always yields the same name (deterministic hash over
        ! the token texts). Codegen currently emits the container as
        ! "program main" and does not print this name, but it is kept stable and
        ! unique for any future consumer that does read it.
        module_name = derive_module_name(tokens)

        ! Parse implicit declarations
        do i = 1, mixed_result%num_implicit_ranges
            range_start = mixed_result%implicit_ranges(i, 1)
            range_end = mixed_result%implicit_ranges(i, 2)

            ! Extract tokens for this range
            if (range_end >= range_start .and. range_end <= size(tokens)) then
                range_tokens = tokens(range_start:range_end)

                ! Parse this declaration range
                call parse_declaration_range(range_tokens, arena, &
                    stmt_index, error_msg, diagnostic_sink)

                if (len_trim(error_msg) > 0) then
                    return
                end if

                if (stmt_index > 0) then
                    implicit_indices = [implicit_indices, stmt_index]
                end if
            end if
        end do

        ! Parse explicit programs
        do i = 1, mixed_result%num_explicit_ranges
            range_start = mixed_result%explicit_ranges(i, 1)
            range_end = mixed_result%explicit_ranges(i, 2)

            ! Extract tokens for this range
            if (range_end >= range_start .and. range_end <= size(tokens)) then
                range_tokens = tokens(range_start:range_end)

                ! Parse this program range
                call parse_program_range(range_tokens, arena, stmt_index, error_msg, &
                    diagnostic_sink)

                if (len_trim(error_msg) > 0) then
                    return
                end if

                if (stmt_index > 0) then
                    explicit_indices = [explicit_indices, stmt_index]
                end if
            end if
        end do

        ! Create the mixed construct container
        num_tokens = size(tokens)
        if (num_tokens > 0) then
            allocate (used_tokens(num_tokens))
            used_tokens = .false.

            call get_environment_variable('FORTFRONT_DEBUG_MIXED', dump_flag, &
                status=env_status)
            if (env_status == 0 .and. len_trim(dump_flag) > 0) then
                write (error_unit, '(A)') 'DEBUG mixed implicit ranges'
                do i = 1, mixed_result%num_implicit_ranges
                    write (error_unit, '(A,1X,I0,1X,I0)') '  range', &
                        mixed_result%implicit_ranges(i, 1), &
                        mixed_result%implicit_ranges(i, 2)
                end do
                write (error_unit, '(A)') 'DEBUG mixed explicit ranges'
                do i = 1, mixed_result%num_explicit_ranges
                    write (error_unit, '(A,1X,I0,1X,I0)') '  range', &
                        mixed_result%explicit_ranges(i, 1), &
                        mixed_result%explicit_ranges(i, 2)
                end do
            end if

            do i = 1, mixed_result%num_implicit_ranges
                range_start = mixed_result%implicit_ranges(i, 1)
                range_end = mixed_result%implicit_ranges(i, 2)
                call mark_token_range(used_tokens, range_start, range_end)
            end do

            do i = 1, mixed_result%num_explicit_ranges
                range_start = mixed_result%explicit_ranges(i, 1)
                range_end = mixed_result%explicit_ranges(i, 2)
                call mark_token_range(used_tokens, range_start, range_end)
            end do

            call parse_remaining_statement_sequences(tokens, used_tokens, arena, &
                implicit_indices, error_msg, diagnostic_sink)
            if (len_trim(error_msg) > 0) return
        end if

        prog_index = create_mixed_construct_container_arena(arena, module_name, &
            implicit_indices, &
            explicit_indices)
    end subroutine parse_mixed_constructs

    ! Parse declaration range
    subroutine parse_declaration_range(tokens, arena, stmt_index, error_msg, &
            diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        character(len=*), intent(out) :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        call parse_tokens_with_dispatcher(tokens, arena, stmt_index, error_msg, &
            diagnostic_sink)
    end subroutine parse_declaration_range

    ! Parse program range
    subroutine parse_program_range(tokens, arena, stmt_index, error_msg, &
            diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        character(len=*), intent(out) :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        call parse_tokens_with_dispatcher(tokens, arena, stmt_index, error_msg, &
            diagnostic_sink)
    end subroutine parse_program_range

    subroutine parse_tokens_with_dispatcher(tokens, arena, stmt_index, error_msg, &
            diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        character(len=*), intent(out) :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        type(token_t), allocatable, target :: stmt_tokens(:)
        type(parser_prefix_buffer_t) :: prefix_buffer

        error_msg = ""

        if (size(tokens) == 0) then
            stmt_index = 0
            return
        end if

        if (tokens(size(tokens))%kind /= TK_EOF) then
            allocate (stmt_tokens(size(tokens) + 1))
            stmt_tokens(1:size(tokens)) = tokens
            stmt_tokens(size(tokens) + 1)%kind = TK_EOF
            stmt_tokens(size(tokens) + 1)%text = ""
            stmt_tokens(size(tokens) + 1)%line = tokens(size(tokens))%line
            stmt_tokens(size(tokens) + 1)%column = tokens(size(tokens))%column + 1
        else
            allocate (stmt_tokens(size(tokens)))
            stmt_tokens = tokens
        end if

        block
            character(len=:), allocatable :: dispatcher_error
            stmt_index = parse_statement_dispatcher(stmt_tokens, arena, &
                prefix_buffer, diagnostic_sink, dispatcher_error)
            if (allocated(dispatcher_error)) error_msg = dispatcher_error
        end block

        deallocate (stmt_tokens)
    end subroutine parse_tokens_with_dispatcher

    ! Create mixed construct container in arena
    function create_mixed_construct_container_arena(arena, module_name, &
            implicit_indices, &
            explicit_indices) &
            result(container_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        integer, intent(in) :: implicit_indices(:)
        integer, intent(in) :: explicit_indices(:)
        integer :: container_index

        type(mixed_construct_container_node) :: container_node

        ! Create the mixed construct container node
        container_node = create_mixed_construct_container(module_name, &
            implicit_indices, &
            explicit_indices)

        ! Add to arena using standard push method
        call arena%push(container_node, "mixed_construct_container", 0)
        container_index = arena%size
    end function create_mixed_construct_container_arena

    ! Deterministic, collision-resistant module name derived from the token
    ! texts (FNV-1a over the concatenated non-empty token text). The same source
    ! always yields the same name; distinct sources almost never collide.
    function derive_module_name(tokens) result(name)
        use, intrinsic :: iso_fortran_env, only: int64
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: name
        integer(int64) :: acc
        integer :: i, j
        character(len=16) :: hex

        acc = 1469598103934665603_int64
        do i = 1, size(tokens)
            if (.not. allocated(tokens(i)%text)) cycle
            do j = 1, len(tokens(i)%text)
                acc = ieor(acc, int(iachar(tokens(i)%text(j:j)), int64))
                acc = acc * 1099511628211_int64
            end do
        end do
        write (hex, '(Z16.16)') acc
        name = "implicit_module_"//trim(adjustl(hex))
    end function derive_module_name

    subroutine mark_token_range(used_tokens, start_idx, end_idx)
        logical, intent(inout) :: used_tokens(:)
        integer, intent(in) :: start_idx, end_idx
        integer :: j

        if (size(used_tokens) == 0) return
        if (start_idx < 1) return
        do j = max(1, start_idx), min(end_idx, size(used_tokens))
            used_tokens(j) = .true.
        end do
    end subroutine mark_token_range

    subroutine parse_remaining_statement_sequences(tokens, used_tokens, arena, &
            implicit_indices, error_msg, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        logical, intent(inout) :: used_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: implicit_indices(:)
        character(len=*), intent(inout) :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: i, seq_start, seq_end, stmt_index
        logical :: in_sequence
        type(token_t), allocatable, target :: segment_tokens(:)

        error_msg = ""
        if (size(tokens) == 0) return

        in_sequence = .false.
        seq_start = 0
        do i = 1, size(tokens)
            if (.not. used_tokens(i) .and. token_has_content(tokens(i))) then
                if (.not. in_sequence) then
                    in_sequence = .true.
                    seq_start = i
                end if
            else
                if (in_sequence) then
                    seq_end = i - 1
                    call parse_sequence(tokens, seq_start, seq_end, arena, &
                        stmt_index, error_msg, diagnostic_sink)
                    if (len_trim(error_msg) > 0) return
                    if (stmt_index > 0) call append_implicit(implicit_indices, &
                        stmt_index)
                    call mark_token_range(used_tokens, seq_start, seq_end)
                    in_sequence = .false.
                end if
            end if
        end do

        if (in_sequence) then
            seq_end = size(tokens)
            call parse_sequence(tokens, seq_start, seq_end, arena, stmt_index, &
                error_msg, diagnostic_sink)
            if (len_trim(error_msg) > 0) return
            if (stmt_index > 0) call append_implicit(implicit_indices, stmt_index)
            call mark_token_range(used_tokens, seq_start, seq_end)
        end if
    contains
        logical function token_has_content(token)
            type(token_t), intent(in) :: token

            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE, TK_EOF)
                token_has_content = .false.
            case default
                token_has_content = len_trim(token%text) > 0
            end select
        end function token_has_content

        subroutine parse_sequence(all_tokens, start_idx, end_idx, arena, &
                stmt_index, error_msg, diagnostic_sink)
            type(token_t), intent(in) :: all_tokens(:)
            integer, intent(in) :: start_idx, end_idx
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(out) :: stmt_index
            character(len=*), intent(inout) :: error_msg
            type(error_collection_t), target, intent(inout), optional :: &
                diagnostic_sink
            type(token_t), allocatable, target :: local_tokens(:)

            stmt_index = 0
            if (start_idx < 1 .or. end_idx < start_idx) return

            local_tokens = all_tokens(start_idx:end_idx)
            call parse_tokens_with_dispatcher(local_tokens, arena, stmt_index, &
                error_msg, diagnostic_sink)
        end subroutine parse_sequence

        subroutine append_implicit(implicit_indices, stmt_index)
            integer, allocatable, intent(inout) :: implicit_indices(:)
            integer, intent(in) :: stmt_index

            if (stmt_index <= 0) return
            implicit_indices = [implicit_indices, stmt_index]
        end subroutine append_implicit
    end subroutine parse_remaining_statement_sequences

end module frontend_mixed_constructs
