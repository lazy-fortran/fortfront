module semantic_annotation_utils
    use string_utils_mod, only: to_lower
    use type_system_unified, only: create_mono_type, mono_type_t, TINT, TREAL, &
        TCHAR, TLOGICAL, TCOMPLEX, TDOUBLE, TDERIVED
    use parser_type_hooks_module, only: type_annotation_t
    implicit none
    private

    public :: type_from_annotation

contains

    subroutine type_from_annotation(annotation, var_type)
        type(type_annotation_t), intent(in) :: annotation
        type(mono_type_t), intent(out) :: var_type
        integer :: kind_id
        character(len=:), allocatable :: lowered

        lowered = adjustl(to_lower(trim(annotation%type_name)))
        select case (lowered)
        case ("integer")
            kind_id = TINT
        case ("real")
            kind_id = TREAL
        case ("character")
            kind_id = TCHAR
        case ("logical")
            kind_id = TLOGICAL
        case ("complex")
            kind_id = TCOMPLEX
        case ("double precision")
            kind_id = TDOUBLE
        case default
            if (index(lowered, "type(") == 1) then
                kind_id = TDERIVED
            else
                kind_id = TREAL
            end if
        end select

        if (kind_id == TINT) then
            var_type = create_mono_type(kind_id, &
                is_unsigned=annotation%is_unsigned)
        else
            var_type = create_mono_type(kind_id)
        end if

        if (annotation%has_kind) then
            if (kind_id == TCHAR) then
                if (annotation%kind_value > 0) then
                    var_type%size = annotation%kind_value
                else if (annotation%kind_value == -1) then
                    var_type%size = -1
                end if
            end if
            var_type%kind = kind_id
        end if
    end subroutine type_from_annotation

end module semantic_annotation_utils
