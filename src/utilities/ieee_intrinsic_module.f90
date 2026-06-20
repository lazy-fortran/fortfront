module ieee_intrinsic_module
    ! F2003 intrinsic modules IEEE_ARITHMETIC, IEEE_EXCEPTIONS, IEEE_FEATURES
    ! (ISO/IEC 1539-1:2004 Section 14). Recognizes the modules on USE and
    ! classifies their public procedures so type inference and validation work.
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: is_ieee_intrinsic_module
    public :: is_ieee_function
    public :: is_ieee_subroutine
    public :: ieee_function_return_type

    integer, parameter :: NAME_LEN = 32

    character(len=NAME_LEN), parameter :: ieee_module_names(3) = &
                                          [character(len=NAME_LEN) :: &
                                  "ieee_arithmetic", "ieee_exceptions", "ieee_features"]

    ! Functions returning a real value.
    character(len=NAME_LEN), parameter :: ieee_real_functions(12) = &
                                          [character(len=NAME_LEN) :: &
                                "ieee_copy_sign", "ieee_fma", "ieee_logb", "ieee_max", &
                        "ieee_max_num", "ieee_min", "ieee_min_num", "ieee_next_after", &
                                    "ieee_rem", "ieee_rint", "ieee_scalb", "ieee_value"]

    ! Functions returning a logical value.
    character(len=NAME_LEN), parameter :: ieee_logical_functions(15) = &
                                          [character(len=NAME_LEN) :: &
                                  "ieee_is_finite", "ieee_is_nan", "ieee_is_negative", &
                                   "ieee_is_normal", "ieee_signbit", "ieee_unordered", &
                                     "ieee_support_datatype", "ieee_support_denormal", &
                   "ieee_support_divide", "ieee_support_flag", "ieee_support_halting", &
                      "ieee_support_inf", "ieee_support_nan", "ieee_support_rounding", &
                                           "ieee_support_standard"]

    ! Functions returning a derived type (ieee_class_type / ieee_round_type).
    character(len=NAME_LEN), parameter :: ieee_derived_functions(2) = &
               [character(len=NAME_LEN) :: "ieee_class", "ieee_get_rounding_mode_value"]

    ! Subroutines (IEEE_GET_*/IEEE_SET_* state manipulation).
    character(len=NAME_LEN), parameter :: ieee_subroutines(8) = &
                                          [character(len=NAME_LEN) :: &
                   "ieee_get_flag", "ieee_get_halting_mode", "ieee_get_rounding_mode", &
                          "ieee_get_status", "ieee_set_flag", "ieee_set_halting_mode", &
                                           "ieee_set_rounding_mode", "ieee_set_status"]

contains

    logical function is_ieee_intrinsic_module(name) result(is_ieee)
        character(len=*), intent(in) :: name

        is_ieee = name_in_list(name, ieee_module_names)
    end function is_ieee_intrinsic_module

    logical function is_ieee_function(name) result(is_func)
        character(len=*), intent(in) :: name

        is_func = name_in_list(name, ieee_real_functions) .or. &
                  name_in_list(name, ieee_logical_functions) .or. &
                  name_in_list(name, ieee_derived_functions)
    end function is_ieee_function

    logical function is_ieee_subroutine(name) result(is_sub)
        character(len=*), intent(in) :: name

        is_sub = name_in_list(name, ieee_subroutines)
    end function is_ieee_subroutine

    function ieee_function_return_type(name) result(return_type)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: return_type

        if (name_in_list(name, ieee_real_functions)) then
            return_type = "real"
        else if (name_in_list(name, ieee_logical_functions)) then
            return_type = "logical"
        else if (name_in_list(name, ieee_derived_functions)) then
            return_type = "derived"
        else
            return_type = ""
        end if
    end function ieee_function_return_type

    logical function name_in_list(name, list) result(found)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: list(:)

        integer :: i
        character(len=:), allocatable :: lowered

        found = .false.
        if (len_trim(name) == 0) return
        lowered = to_lower(trim(name))

        do i = 1, size(list)
            if (trim(list(i)) == lowered) then
                found = .true.
                return
            end if
        end do
    end function name_in_list

end module ieee_intrinsic_module
