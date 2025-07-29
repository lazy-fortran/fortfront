module type_checker
    ! Type checking and compatibility rules for Fortran
    use type_system_hm
    implicit none
    private

    public :: is_assignable, is_numeric_type, is_compatible
    public :: check_argument_types, check_array_conformance
    public :: get_common_type, type_error

    ! Type compatibility levels
    integer, parameter, public :: COMPAT_EXACT = 1      ! Exact type match
    integer, parameter, public :: COMPAT_PROMOTE = 2    ! Type promotion allowed
    integer, parameter, public :: COMPAT_CONVERT = 3    ! Type conversion allowed
    integer, parameter, public :: COMPAT_NONE = 0       ! Not compatible

contains

    ! Check if a value of type 'from' can be assigned to variable of type 'to'
    recursive function is_assignable(from_type, to_type) result(assignable)
        type(mono_type_t), intent(in) :: from_type, to_type
        logical :: assignable

        ! Handle type variables
        if (from_type%kind == TVAR .or. to_type%kind == TVAR) then
            assignable = .true.  ! Type variables can unify with anything
            return
        end if

        ! Same type is always assignable
        if (from_type%kind == to_type%kind) then
            select case (from_type%kind)
            case (TCHAR)
                ! Character types must have compatible lengths
                assignable = (to_type%size >= from_type%size)
            case (TARRAY)
                ! Arrays must have compatible element types and sizes
                if (allocated(from_type%args) .and. allocated(to_type%args)) then
                    assignable = is_assignable(from_type%args(1), to_type%args(1))
                    if (assignable .and. from_type%size > 0 .and. to_type%size > 0) then
                        assignable = (from_type%size == to_type%size)
                    end if
                else
                    assignable = .false.
                end if
            case default
                assignable = .true.
            end select
            return
        end if

        ! Check type promotion rules
        select case (to_type%kind)
        case (TREAL)
            ! Integer can be promoted to real
            assignable = (from_type%kind == TINT)
        case (TCHAR)
            ! Nothing promotes to character
            assignable = .false.
        case default
            assignable = .false.
        end select

    end function is_assignable

    ! Check if a type is numeric (integer or real)
    function is_numeric_type(typ) result(is_numeric)
        type(mono_type_t), intent(in) :: typ
        logical :: is_numeric

        is_numeric = (typ%kind == TINT .or. typ%kind == TREAL)

    end function is_numeric_type

    ! Check if two types are compatible (for operations)
    function is_compatible(type1, type2, level) result(compatible)
        type(mono_type_t), intent(in) :: type1, type2
        integer, intent(out) :: level
        logical :: compatible

        level = COMPAT_NONE
        compatible = .false.

        ! Type variables are always compatible
        if (type1%kind == TVAR .or. type2%kind == TVAR) then
            level = COMPAT_EXACT
            compatible = .true.
            return
        end if

        ! Same type is exactly compatible
        if (type1%kind == type2%kind) then
            level = COMPAT_EXACT
            compatible = .true.
            return
        end if

        ! Numeric type compatibility
        if (is_numeric_type(type1) .and. is_numeric_type(type2)) then
            level = COMPAT_PROMOTE
            compatible = .true.
            return
        end if

    end function is_compatible

    ! Get the common type for two compatible types
    function get_common_type(type1, type2) result(common_type)
        type(mono_type_t), intent(in) :: type1, type2
        type(mono_type_t) :: common_type
        integer :: compat_level

        ! If either is a type variable, return the other
        if (type1%kind == TVAR) then
            common_type = type2
            return
        else if (type2%kind == TVAR) then
            common_type = type1
            return
        end if

        ! If same type, return it
        if (type1%kind == type2%kind) then
            common_type = type1
            return
        end if

        ! For numeric types, real is the common type
        if (is_numeric_type(type1) .and. is_numeric_type(type2)) then
            common_type = create_mono_type(TREAL)
            return
        end if

        ! Otherwise, no common type
        common_type%kind = 0  ! Invalid type

    end function get_common_type

    ! Check argument types match parameter types
    function check_argument_types(arg_types, param_types, allow_promotion) result(match)
        type(mono_type_t), intent(in) :: arg_types(:), param_types(:)
        logical, intent(in) :: allow_promotion
        logical :: match
        integer :: i

        match = .false.

        ! Check arity
        if (size(arg_types) /= size(param_types)) return

        ! Check each argument
        do i = 1, size(arg_types)
            if (allow_promotion) then
                if (.not. is_assignable(arg_types(i), param_types(i))) return
            else
                if (arg_types(i)%kind /= param_types(i)%kind) return
            end if
        end do

        match = .true.

    end function check_argument_types

    ! Check array conformance
    function check_array_conformance(array1, array2) result(conformant)
        type(mono_type_t), intent(in) :: array1, array2
        logical :: conformant

        conformant = .false.

        ! Both must be arrays
        if (array1%kind /= TARRAY .or. array2%kind /= TARRAY) return

        ! Check element types are compatible
        if (allocated(array1%args) .and. allocated(array2%args)) then
            if (.not. is_assignable(array1%args(1), array2%args(1))) return
        else
            return
        end if

        ! Check sizes (0 means dynamic/unknown size)
        if (array1%size > 0 .and. array2%size > 0) then
            conformant = (array1%size == array2%size)
        else
            conformant = .true.  ! Dynamic arrays assumed conformant
        end if

    end function check_array_conformance

    ! Generate type error message
    function type_error(expected, actual, context) result(msg)
        type(mono_type_t), intent(in) :: expected, actual
        character(len=*), intent(in) :: context
        character(len=:), allocatable :: msg

        msg = "Type error in "//trim(context)//": expected "// &
              expected%to_string()//" but got "//actual%to_string()

    end function type_error

end module type_checker
