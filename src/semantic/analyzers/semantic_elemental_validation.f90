module semantic_elemental_validation
    ! Validates ELEMENTAL procedure dummy arguments per F2008 C1290.
    ! Every dummy argument of an ELEMENTAL procedure must be scalar; an array
    ! dummy is prohibited. An explicit IMPURE prefix relaxes purity, but the
    ! scalar-dummy requirement still applies to ELEMENTAL procedures.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_elemental_procedure, is_elemental_prefix

contains

    ! Returns .true. when the prefix list marks the procedure as ELEMENTAL.
    function is_elemental_prefix(prefix_keywords) result(is_elemental)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical :: is_elemental
        integer :: i

        is_elemental = .false.
        if (.not. allocated(prefix_keywords)) return

        do i = 1, size(prefix_keywords)
            if (trim(prefix_keywords(i)) == 'elemental') then
                is_elemental = .true.
                return
            end if
        end do
    end function is_elemental_prefix

    ! Validate the dummy arguments of an ELEMENTAL procedure. Non-elemental
    ! procedures are accepted unchanged.
    subroutine validate_elemental_procedure(arena, param_indices, body_indices, &
                                            prefix_keywords, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i
        character(len=:), allocatable :: dummy_name

        if (.not. is_elemental_prefix(prefix_keywords)) return
        if (.not. allocated(param_indices)) return

        do i = 1, size(param_indices)
            if (param_is_array(arena, param_indices(i))) then
                call report_array_dummy(errors, arena, param_indices(i))
                cycle
            end if
            dummy_name = param_name(arena, param_indices(i))
            if (len_trim(dummy_name) == 0) cycle
            if (.not. allocated(body_indices)) cycle
            if (body_declares_array(arena, body_indices, dummy_name)) then
                call report_array_dummy(errors, arena, param_indices(i))
            end if
        end do
    end subroutine validate_elemental_procedure

    ! Whether the parameter node itself carries an array specification. Standard
    ! Fortran params are identifier nodes; the array spec then lives in a body
    ! declaration. Inline declaration params carry is_array directly.
    function param_is_array(arena, param_index) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        logical :: is_array

        is_array = .false.
        if (.not. arena%has_node_at(param_index)) return

        select type (node => arena%entries(param_index)%node)
        type is (declaration_node)
            is_array = node%is_array
        type is (parameter_declaration_node)
            is_array = node%is_array
        end select
    end function param_is_array

    ! Extract the dummy argument name from a parameter node.
    function param_name(arena, param_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=:), allocatable :: name

        name = ''
        if (.not. arena%has_node_at(param_index)) return

        select type (node => arena%entries(param_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
        type is (declaration_node)
            if (allocated(node%var_name)) name = trim(node%var_name)
        type is (parameter_declaration_node)
            if (allocated(node%name)) name = trim(node%name)
        end select
    end function param_name

    ! Whether a body declaration declares dummy_name as an array.
    function body_declares_array(arena, body_indices, dummy_name) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: dummy_name
        logical :: is_array
        integer :: i

        is_array = .false.
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
            type is (declaration_node)
                if (.not. node%is_array) cycle
                if (declaration_names_dummy(node, dummy_name)) then
                    is_array = .true.
                    return
                end if
            end select
        end do
    end function body_declares_array

    ! Whether a declaration node names dummy_name among its declared variables.
    function declaration_names_dummy(node, dummy_name) result(matches)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: dummy_name
        logical :: matches
        integer :: i

        matches = .false.
        if (allocated(node%var_name)) then
            if (trim(node%var_name) == trim(dummy_name)) then
                matches = .true.
                return
            end if
        end if
        if (allocated(node%var_names)) then
            do i = 1, size(node%var_names)
                if (trim(node%var_names(i)) == trim(dummy_name)) then
                    matches = .true.
                    return
                end if
            end do
        end if
    end function declaration_names_dummy

    subroutine report_array_dummy(errors, arena, param_index)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        integer :: line, column

        line = 0
        column = 0
        if (arena%has_node_at(param_index)) then
            line = arena%entries(param_index)%node%line
            column = arena%entries(param_index)%node%column
        end if

        call errors%add_error( &
            message='array dummy argument is not allowed in an ELEMENTAL '// &
            'procedure', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='make the dummy argument scalar, or drop the '// &
            'ELEMENTAL prefix')
    end subroutine report_array_dummy

end module semantic_elemental_validation
