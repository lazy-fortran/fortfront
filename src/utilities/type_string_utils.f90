module type_string_utils
    use type_system_unified, only: mono_type_t, type_args_allocated, &
                                   type_args_size, type_args_element, TVAR, &
                                   TINT, TREAL, TCHAR, TLOGICAL, TARRAY, &
                                   TCOMPLEX, TDOUBLE, TFUN, TDERIVED
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: is_character_type_string
    public :: mono_type_to_string

contains

    pure logical function is_character_type_string(type_str) result(is_character)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: lowered

        trimmed = adjustl(trim(type_str))
        if (len_trim(trimmed) < len("character")) then
            is_character = .false.
            return
        end if

        lowered = to_lower(trimmed)
        is_character = index(lowered, "character") == 1
    end function is_character_type_string

    recursive function mono_type_to_string(mono_type, include_shape, &
                                           prefer_len_zero_char, &
                                           standardize_real, fallback, &
                                           success) result(type_str)
        type(mono_type_t), intent(in) :: mono_type
        logical, intent(in), optional :: include_shape
        logical, intent(in), optional :: prefer_len_zero_char
        logical, intent(in), optional :: standardize_real
        character(len=*), intent(in), optional :: fallback
        logical, intent(out), optional :: success
        character(len=:), allocatable :: type_str

        logical :: include_shape_local
        logical :: prefer_len_zero_local
        logical :: standardize_real_local
        logical :: success_local
        character(len=:), allocatable :: computed_string

        include_shape_local = .false.
        if (present(include_shape)) include_shape_local = include_shape

        prefer_len_zero_local = .false.
        if (present(prefer_len_zero_char)) &
            prefer_len_zero_local = prefer_len_zero_char

        standardize_real_local = .false.
        if (present(standardize_real)) standardize_real_local = standardize_real

        call resolve_mono_type_string(mono_type, include_shape_local, &
                                      prefer_len_zero_local, &
                                      standardize_real_local, computed_string, &
                                      success_local)

        if (success_local) then
            type_str = computed_string
        else if (present(fallback)) then
            type_str = fallback
        else
            type_str = ""
        end if

        if (present(success)) success = success_local
    end function mono_type_to_string

    recursive subroutine resolve_mono_type_string(mono_type, include_shape, &
                                                  prefer_len_zero_char, &
                                                  standardize_real, &
                                                  type_str, success)
        type(mono_type_t), intent(in) :: mono_type
        logical, intent(in) :: include_shape
        logical, intent(in) :: prefer_len_zero_char
        logical, intent(in) :: standardize_real
        character(len=:), allocatable, intent(out) :: type_str
        logical, intent(out) :: success

        character(len=:), allocatable :: array_string
        success = .true.; type_str = ""

        if (mono_type%kind <= 0) then
            success = .false.
            return
        end if

        select case (mono_type%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            type_str = "real"
            if (standardize_real) type_str = "real(8)"
        case (TLOGICAL)
            type_str = "logical"
        case (TCHAR)
            call resolve_character_string(mono_type, prefer_len_zero_char, &
                                          type_str)
        case (TARRAY)
            call resolve_array_string(mono_type, include_shape, &
                                      prefer_len_zero_char, standardize_real, &
                                      array_string, success)
            if (success) type_str = array_string
        case (TCOMPLEX)
            type_str = "complex"
        case (TDOUBLE)
            type_str = "double precision"
        case (TFUN)
            type_str = "function"
        case (TVAR)
            type_str = "type_variable"
        case (TDERIVED)
            type_str = "derived_type"
        case default
            success = .false.
        end select
    end subroutine resolve_mono_type_string

    subroutine resolve_character_string(mono_type, prefer_len_zero_char, type_str)
        type(mono_type_t), intent(in) :: mono_type
        logical, intent(in) :: prefer_len_zero_char
        character(len=:), allocatable, intent(out) :: type_str

        character(len=32) :: size_buffer

        if (mono_type%alloc_info%needs_allocatable_string) then
            type_str = "character(len=:), allocatable"
        else if (mono_type%size > 0) then
            write (size_buffer, '(i0)') mono_type%size
            type_str = "character(len=" // trim(size_buffer) // ")"
        else
            if (prefer_len_zero_char) then
                type_str = "character(len=0)"
            else
                type_str = "character(len=:)"
            end if
        end if
    end subroutine resolve_character_string

    recursive subroutine resolve_array_string(mono_type, include_shape, &
                                              prefer_len_zero_char, &
                                              standardize_real, type_str, &
                                              success)
        type(mono_type_t), intent(in) :: mono_type
        logical, intent(in) :: include_shape
        logical, intent(in) :: prefer_len_zero_char
        logical, intent(in) :: standardize_real
        character(len=:), allocatable, intent(out) :: type_str
        logical, intent(out) :: success

        character(len=:), allocatable :: element_str
        logical :: element_success
        character(len=32) :: size_buffer

        success = .true.
        type_str = ""

        if (.not. type_args_allocated(mono_type)) then
            success = .false.
            return
        end if

        if (type_args_size(mono_type) <= 0) then
            success = .false.
            return
        end if

        element_str = mono_type_to_string(type_args_element(mono_type, 1), &
                                          include_shape=.false., &
                                          prefer_len_zero_char=prefer_len_zero_char, &
                                          standardize_real=standardize_real, &
                                          success=element_success)
        if (.not. element_success) then
            success = .false.
            return
        end if

        if (include_shape) then
            if (mono_type%size > 0) then
                write (size_buffer, '(i0)') mono_type%size
                type_str = trim(element_str) // ", dimension(" // &
                    trim(size_buffer) // ")"
            else if (mono_type%alloc_info%is_allocatable .or. &
                     mono_type%alloc_info%needs_allocatable_string) then
                type_str = trim(element_str) // ", dimension(:), allocatable"
            else
                type_str = trim(element_str) // ", dimension(:)"
            end if
        else
            type_str = element_str
        end if
    end subroutine resolve_array_string

end module type_string_utils
