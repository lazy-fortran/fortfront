module semantic_call_signature_collector
    use, intrinsic :: iso_fortran_env, only: error_unit
    use debug_trace, only: trace_is_enabled
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node
    use ast_nodes_procedure, only: subroutine_call_node
    use call_graph_signatures_mod, only: add_signature, signatures_map_t
    use type_system_unified, only: mono_type_t, create_mono_type, TINT, TREAL, &
                                   TLOGICAL, TCHAR, TCOMPLEX, TDOUBLE, TARRAY
    use type_string_utils, only: mono_type_to_string
    use string_utils_mod, only: to_lower
    use semantic_validation_utils, only: int_to_str
    implicit none
    private

    public :: collect_call_signature, collect_subroutine_signature

contains

    subroutine collect_call_signature(signatures, arena, call_node, return_type, &
                                      node_index)
        type(signatures_map_t), intent(inout) :: signatures
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        type(mono_type_t), intent(inout) :: return_type
        integer, intent(in) :: node_index
        integer, allocatable :: param_kinds(:)
        character(len=:), allocatable :: param_type_strings(:)
        character(len=:), allocatable :: return_type_tmp
        integer :: return_kind
        integer :: n_args
        integer :: deduced_kind
        integer :: param_kind
        type(mono_type_t) :: return_type_copy

        if (.not. allocated(call_node%name)) return

        call extract_parameter_info(arena, call_node%arg_indices, param_kinds, &
                                    param_type_strings, n_args)

        return_kind = return_type%get_kind()
        return_type_copy = return_type
        call return_type_copy%sync_from_arena()
        return_type_tmp = mono_type_to_string(return_type_copy, &
                                              include_shape=.true., fallback='')
        if (len_trim(return_type_tmp) == 0) then
            return_type_tmp = kind_to_type_string(return_kind)
        end if
        return_type_tmp = trim(return_type_tmp)

        deduced_kind = return_kind
        if (deduced_kind <= 0) deduced_kind = return_type_copy%kind
        if (allocated(param_kinds)) then
            param_kind = dominant_kind_from_params(param_kinds)
        else
            param_kind = 0
        end if
        if (param_kind > 0) then
            if (deduced_kind <= 0) then
                deduced_kind = param_kind
            else if (deduced_kind == TREAL .and. param_kind /= TREAL) then
                deduced_kind = param_kind
            else if (deduced_kind == TINT .and. param_kind /= TINT) then
                deduced_kind = param_kind
            end if
        end if
        if (deduced_kind <= 0) deduced_kind = type_string_to_kind(return_type_tmp)
        ! Only update return_type if it's NOT already an array type (Issue #2153)
        ! Array return types should be preserved from earlier inference
        if (return_type%kind /= TARRAY) then
            select case (deduced_kind)
            case (TINT, TREAL, TLOGICAL, TCHAR, TCOMPLEX, TDOUBLE)
                return_type = create_mono_type(deduced_kind)
            end select
        end if

        if (n_args > 0) then
            if (len(return_type_tmp) > 0) then
                call add_signature(signatures, call_node%name, param_kinds, &
                                   deduced_kind, node_index, 0, 0, &
                                   param_type_strings, return_type_tmp)
            else
                call add_signature(signatures, call_node%name, param_kinds, &
                                   deduced_kind, node_index, 0, 0, &
                                   param_type_strings=param_type_strings)
            end if
        else
            if (len(return_type_tmp) > 0) then
                call add_signature(signatures, call_node%name, param_kinds, &
                                   deduced_kind, node_index, 0, 0, &
                                   return_type_string=return_type_tmp)
            else
                call add_signature(signatures, call_node%name, param_kinds, &
                                   deduced_kind, node_index, 0, 0)
            end if
        end if
    end subroutine collect_call_signature

    subroutine collect_subroutine_signature(signatures, arena, call_node, &
                                            node_index)
        type(signatures_map_t), intent(inout) :: signatures
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_call_node), intent(in) :: call_node
        integer, intent(in) :: node_index
        integer, allocatable :: param_kinds(:)
        character(len=:), allocatable :: param_type_strings(:)
        integer :: n_args

        if (.not. allocated(call_node%name)) return

        call extract_parameter_info(arena, call_node%arg_indices, param_kinds, &
                                    param_type_strings, n_args)

        if (trace_is_enabled()) then
            write (error_unit, '(A,1X,A,1X,I0)') &
                'DEBUG collect_subroutine_signature name=', &
                trim(call_node%name), n_args
            if (allocated(param_kinds)) then
                write (error_unit, '(A,*(1X,I0))') 'DEBUG kinds=', param_kinds
            else
                write (error_unit, '(A)') 'DEBUG kinds=<not allocated>'
            end if
            if (allocated(param_type_strings)) then
                write (error_unit, '(A,*(1X,A))') 'DEBUG types=', param_type_strings
            else
                write (error_unit, '(A)') 'DEBUG types=<not allocated>'
            end if
        end if

        if (n_args > 0) then
            call add_signature(signatures, call_node%name, param_kinds, 0, &
                               node_index, 0, 0, &
                               param_type_strings=param_type_strings)
        else
            call add_signature(signatures, call_node%name, param_kinds, 0, &
                               node_index, 0, 0)
        end if
    end subroutine collect_subroutine_signature

    subroutine extract_parameter_info(arena, arg_indices, param_kinds, &
                                      param_type_strings, n_args)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: arg_indices(:)
        integer, allocatable, intent(out) :: param_kinds(:)
        character(len=:), allocatable, intent(out) :: param_type_strings(:)
        integer, intent(out) :: n_args
        integer :: i
        type(mono_type_t) :: arg_type
        character(len=:), allocatable :: param_type_tmp

        if (allocated(param_type_strings)) deallocate (param_type_strings)

        if (.not. allocated(arg_indices)) then
            n_args = 0
            allocate (param_kinds(0))
            return
        end if

        n_args = size(arg_indices)
        allocate (param_kinds(n_args))
        if (n_args > 0) allocate (character(len=64) :: param_type_strings(n_args))

        do i = 1, n_args
            arg_type = get_inferred_type_from_arena_local(arena, arg_indices(i))
            call arg_type%sync_from_arena()
            param_kinds(i) = arg_type%get_kind()
            param_type_tmp = mono_type_to_string(arg_type, include_shape=.true., &
                                                 fallback='')
            if (len_trim(param_type_tmp) == 0) then
                param_type_tmp = kind_to_type_string(param_kinds(i))
            end if
            if (allocated(param_type_strings)) then
                param_type_strings(i) = trim(param_type_tmp)
            end if
        end do
    end subroutine extract_parameter_info

    function get_inferred_type_from_arena_local(a, index) result(typ)
        type(ast_arena_t), intent(inout) :: a
        integer, intent(in) :: index
        type(mono_type_t) :: typ

        typ = create_mono_type(TREAL)
        if (index <= 0 .or. index > a%size) return
        if (.not. allocated(a%entries(index)%node)) return

        typ = a%entries(index)%node%inferred_type
        if (typ%kind == 1) then
            if (len_trim(typ%var%name) == 0) typ%var%name = "v" // &
                                                            int_to_str(typ%var%id)
        end if
        a%entries(index)%node%inferred_type = typ
    end function get_inferred_type_from_arena_local

    function kind_to_type_string(kind_value) result(str)
        integer, intent(in) :: kind_value
        character(len=:), allocatable :: str

        select case (kind_value)
        case (TINT)
            str = "integer"
        case (TREAL)
            str = "real"
        case (TLOGICAL)
            str = "logical"
        case (TCHAR)
            str = "character"
        case (TCOMPLEX)
            str = "complex"
        case (TDOUBLE)
            str = "real(8)"
        case (TARRAY)
            str = "array"
        case default
            str = ""
        end select
    end function kind_to_type_string

    integer function type_string_to_kind(type_name) result(kind_value)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(type_name))
        select case (lowered)
        case ('integer')
            kind_value = TINT
        case ('real')
            kind_value = TREAL
        case ('logical')
            kind_value = TLOGICAL
        case ('complex')
            kind_value = TCOMPLEX
        case ('real(8)', 'double precision')
            kind_value = TDOUBLE
        case default
            if (index(lowered, 'character') == 1) then
                kind_value = TCHAR
            else if (index(lowered, 'real(8)') == 1) then
                kind_value = TDOUBLE
            else if (index(lowered, 'real(') == 1) then
                kind_value = TREAL
            else if (index(lowered, 'integer') == 1) then
                kind_value = TINT
            else
                kind_value = 0
            end if
        end select
    end function type_string_to_kind

    integer function dominant_kind_from_params(param_kinds) result(kind_value)
        integer, intent(in) :: param_kinds(:)
        integer :: i
        integer :: best_rank

        kind_value = 0
        best_rank = -1

        do i = 1, size(param_kinds)
            select case (param_kinds(i))
            case (TDOUBLE)
                kind_value = TDOUBLE
                return
            case (TCOMPLEX)
                if (best_rank < 3) then
                    kind_value = TCOMPLEX
                    best_rank = 3
                end if
            case (TREAL)
                if (best_rank < 2) then
                    kind_value = TREAL
                    best_rank = 2
                end if
            case (TCHAR)
                if (best_rank < 2) then
                    kind_value = TCHAR
                    best_rank = 2
                end if
            case (TLOGICAL)
                if (best_rank < 1) then
                    kind_value = TLOGICAL
                    best_rank = 1
                end if
            case (TINT)
                if (best_rank < 0) then
                    kind_value = TINT
                    best_rank = 0
                end if
            case default
                if (best_rank < 0) then
                    kind_value = param_kinds(i)
                    best_rank = 0
                end if
            end select
        end do
    end function dominant_kind_from_params

end module semantic_call_signature_collector
