module semantic_pure_validation
    ! Validates PURE/ELEMENTAL procedure bodies per F2008 C1283.
    ! A PURE procedure must not perform external I/O (PRINT, WRITE to an
    ! external unit, READ) or execute an image-control / program-halting
    ! statement such as STOP or PAUSE. ELEMENTAL implies PURE, so the same
    ! body restrictions apply.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
        read_statement_node
    use ast_nodes_transfer, only: stop_node, pause_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_conditional, only: if_node
    use ast_nodes_associate, only: block_construct_node
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use semantic_pure_dummy_validation, only: validate_pure_dummies
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    public :: validate_pure_procedure, is_pure_prefix
    public :: validate_do_concurrent_purity

    ! F2008 13.1: MVBITS and MOVE_ALLOC are the only PURE intrinsic
    ! subroutines. Coarray and atomic intrinsics are deliberately left out of
    ! this list; they belong to a different diagnostic family.
    integer, parameter :: IMPURE_INTRINSIC_NAME_LEN = 24
    character(len=IMPURE_INTRINSIC_NAME_LEN), parameter :: &
        IMPURE_INTRINSIC_SUBROUTINES(10) = [ &
        character(len=IMPURE_INTRINSIC_NAME_LEN) :: &
        'cpu_time', 'date_and_time', 'execute_command_line', 'get_command', &
        'get_command_argument', 'get_environment_variable', 'random_init', &
        'random_number', 'random_seed', 'system_clock']

contains

    ! Returns .true. when the prefix list marks the procedure as PURE,
    ! either explicitly (pure) or implicitly (elemental). An explicit
    ! impure keyword overrides elemental.
    function is_pure_prefix(prefix_keywords) result(is_pure)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical :: is_pure
        logical :: has_pure, has_elemental, has_impure
        integer :: i

        is_pure = .false.
        if (.not. allocated(prefix_keywords)) return

        has_pure = .false.
        has_elemental = .false.
        has_impure = .false.
        do i = 1, size(prefix_keywords)
            select case (trim(prefix_keywords(i)))
            case ('pure')
                has_pure = .true.
            case ('elemental')
                has_elemental = .true.
            case ('impure')
                has_impure = .true.
            end select
        end do

        if (has_impure) return
        is_pure = has_pure .or. has_elemental
    end function is_pure_prefix

    ! Validate a procedure body when its prefix marks it PURE/ELEMENTAL.
    ! Non-pure procedures are accepted unchanged.
    subroutine validate_pure_procedure(arena, body_indices, prefix_keywords, &
            errors, param_indices, is_function)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable, intent(in), optional :: param_indices(:)
        logical, intent(in), optional :: is_function
        logical :: function_context

        if (.not. is_pure_prefix(prefix_keywords)) return

        if (present(param_indices)) then
            function_context = .false.
            if (present(is_function)) function_context = is_function
            call validate_pure_dummies(arena, param_indices, body_indices, &
                function_context, errors)
        end if

        if (.not. allocated(body_indices)) return

        call check_pure_body(arena, body_indices, errors)
    end subroutine validate_pure_procedure

    ! F2008 8.1.6.5: the body of a DO CONCURRENT construct is a pure context,
    ! so it may not reference an impure intrinsic subroutine.
    recursive subroutine validate_do_concurrent_purity(arena, body_indices, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        if (.not. allocated(body_indices)) return
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            call check_concurrent_statement(arena, body_indices(i), errors)
        end do
    end subroutine validate_do_concurrent_purity

    recursive subroutine check_concurrent_statement(arena, stmt_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        select type (stmt => arena%entries(stmt_index)%node)
            type is (subroutine_call_node)
            call check_concurrent_call(arena, stmt, errors)
            type is (block_construct_node)
            call validate_do_concurrent_purity(arena, stmt%body_indices, errors)
            type is (do_loop_node)
            call validate_do_concurrent_purity(arena, stmt%body_indices, errors)
            type is (do_while_node)
            call validate_do_concurrent_purity(arena, stmt%body_indices, errors)
            type is (if_node)
            call validate_do_concurrent_purity(arena, stmt%then_body_indices, &
                errors)
            if (allocated(stmt%elseif_blocks)) then
                do i = 1, size(stmt%elseif_blocks)
                    call validate_do_concurrent_purity(arena, &
                        stmt%elseif_blocks(i)%body_indices, errors)
                end do
            end if
            call validate_do_concurrent_purity(arena, stmt%else_body_indices, &
                errors)
        end select
    end subroutine check_concurrent_statement

    ! F2018 C1141: a reference to an impure procedure shall not appear in a
    ! DO CONCURRENT construct.
    subroutine check_concurrent_call(arena, stmt, errors)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_call_node), intent(in) :: stmt
        type(error_collection_t), intent(inout) :: errors
        integer :: def_index

        if (.not. allocated(stmt%name)) return

        if (is_impure_intrinsic_subroutine(stmt%name)) then
            if (.not. procedure_defined_in_arena(arena, stmt%name)) then
                call report_concurrent_call(errors, 'impure intrinsic '// &
                    'subroutine "'//trim(stmt%name)//'"', stmt%line, stmt%column)
                return
            end if
        end if

        def_index = find_subroutine_definition(arena, stmt%name)
        if (def_index <= 0) return
        select type (def => arena%entries(def_index)%node)
            type is (subroutine_def_node)
            if (is_pure_prefix(def%prefix_keywords)) return
            call report_concurrent_call(errors, 'Subroutine call to impure "'// &
                trim(stmt%name)//'"', stmt%line, stmt%column)
        end select
    end subroutine check_concurrent_call

    subroutine report_concurrent_call(errors, subject, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: subject
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=subject//' is not allowed in a DO CONCURRENT construct', &
            code=ERROR_SEMANTIC, &
            component='semantic_pure_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='move the impure call outside the DO CONCURRENT '// &
            'construct, declare the procedure PURE, or use an ordinary DO loop', &
            line=line, column=column, end_line=line, end_column=column + 1)
    end subroutine report_concurrent_call

    ! Arena index of a subroutine definition with this name, or 0.
    function find_subroutine_definition(arena, name) result(def_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: def_index
        character(len=:), allocatable :: lowered
        integer :: i

        def_index = 0
        lowered = to_lower(trim(name))
        if (len_trim(lowered) == 0) return
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (subroutine_def_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) == lowered) then
                    def_index = i
                    return
                end if
            end select
        end do
    end function find_subroutine_definition

    function is_impure_intrinsic_subroutine(name) result(is_impure)
        character(len=*), intent(in) :: name
        logical :: is_impure
        character(len=:), allocatable :: lowered
        integer :: i

        is_impure = .false.
        lowered = to_lower(trim(name))
        do i = 1, size(IMPURE_INTRINSIC_SUBROUTINES)
            if (trim(IMPURE_INTRINSIC_SUBROUTINES(i)) == lowered) then
                is_impure = .true.
                return
            end if
        end do
    end function is_impure_intrinsic_subroutine

    ! A user procedure of the same name shadows the intrinsic, so the call is
    ! not an intrinsic reference and this rule does not apply.
    function procedure_defined_in_arena(arena, name) result(is_defined)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        logical :: is_defined
        character(len=:), allocatable :: lowered
        integer :: i

        is_defined = .false.
        lowered = to_lower(trim(name))
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (subroutine_def_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) == lowered) then
                    is_defined = .true.
                    return
                end if
                type is (function_def_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) == lowered) then
                    is_defined = .true.
                    return
                end if
            end select
        end do
    end function procedure_defined_in_arena

    ! Recursively scan a list of body statements for prohibited statements.
    recursive subroutine check_pure_body(arena, body_indices, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i, stmt_index

        do i = 1, size(body_indices)
            stmt_index = body_indices(i)
            if (.not. arena%has_node_at(stmt_index)) cycle
            call check_pure_statement(arena, stmt_index, errors)
        end do
    end subroutine check_pure_body

    ! Inspect a single statement: report it if prohibited, recurse into the
    ! bodies of nested control-flow constructs otherwise.
    recursive subroutine check_pure_statement(arena, stmt_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        type(error_collection_t), intent(inout) :: errors

        select type (stmt => arena%entries(stmt_index)%node)
            type is (print_statement_node)
            call report_impure_stmt(errors, 'PRINT', stmt%line, stmt%column)
            type is (write_statement_node)
            call report_impure_stmt(errors, 'WRITE', stmt%line, stmt%column)
            type is (read_statement_node)
            call report_impure_stmt(errors, 'READ', stmt%line, stmt%column)
            type is (stop_node)
            call report_impure_stmt(errors, 'STOP', stmt%line, stmt%column)
            type is (pause_node)
            call report_impure_stmt(errors, 'PAUSE', stmt%line, stmt%column)
            type is (if_node)
            call check_pure_if(arena, stmt, errors)
            type is (do_loop_node)
            if (allocated(stmt%body_indices)) &
                call check_pure_body(arena, stmt%body_indices, errors)
            type is (do_while_node)
            if (allocated(stmt%body_indices)) &
                call check_pure_body(arena, stmt%body_indices, errors)
        end select
    end subroutine check_pure_statement

    ! Recurse into all branches of an IF construct.
    recursive subroutine check_pure_if(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        if (allocated(node%then_body_indices)) &
            call check_pure_body(arena, node%then_body_indices, errors)
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                if (allocated(node%elseif_blocks(i)%body_indices)) &
                    call check_pure_body(arena, &
                    node%elseif_blocks(i)%body_indices, errors)
            end do
        end if
        if (allocated(node%else_body_indices)) &
            call check_pure_body(arena, node%else_body_indices, errors)
    end subroutine check_pure_if

    subroutine report_impure_stmt(errors, kind, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: kind
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=kind//' statement is not allowed in a PURE procedure', &
            code=ERROR_SEMANTIC, &
            component='semantic_pure_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='remove the I/O or control statement, or drop the '// &
            'PURE/ELEMENTAL prefix', line=line, column=column, end_line=line, &
            end_column=column + 1)
    end subroutine report_impure_stmt

end module semantic_pure_validation
