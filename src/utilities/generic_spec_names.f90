module generic_spec_names
    ! Canonical spelling of the generic specs that may appear in an INTERFACE
    ! statement and in a USE ONLY list. Fortran gives the relational operators
    ! two spellings (== and .eq., /= and .ne., ...) that denote the SAME
    ! generic spec (F2023 10.1.5.5.1), so every comparison of operator names
    ! goes through normalize_generic_operator, and every stored USE ONLY entry
    ! for a generic spec is built with make_generic_spec.
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: normalize_generic_operator
    public :: make_generic_spec
    public :: is_generic_spec

contains

    ! Fold the two spellings of the relational operators onto one form.
    function normalize_generic_operator(symbol) result(normalized)
        character(len=*), intent(in) :: symbol
        character(len=:), allocatable :: normalized

        normalized = to_lower(trim(symbol))
        select case (normalized)
        case (".gt.")
            normalized = ">"
        case (".lt.")
            normalized = "<"
        case (".ge.")
            normalized = ">="
        case (".le.")
            normalized = "<="
        case (".eq.")
            normalized = "=="
        case (".ne.")
            normalized = "/="
        end select
    end function normalize_generic_operator

    ! Canonical text for a generic spec, e.g. "operator(==)" or
    ! "assignment(=)". kind is "operator" or "assignment".
    function make_generic_spec(kind, symbol) result(spec)
        character(len=*), intent(in) :: kind
        character(len=*), intent(in) :: symbol
        character(len=:), allocatable :: spec

        spec = to_lower(trim(kind))//"("// &
            normalize_generic_operator(symbol)//")"
    end function make_generic_spec

    ! Whether a USE ONLY entry names a generic spec rather than a plain name.
    logical function is_generic_spec(text) result(is_spec)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(text))
        is_spec = .false.
        if (len(lowered) < 3) return
        if (lowered(len(lowered):len(lowered)) /= ")") return
        if (len(lowered) > 9) then
            if (lowered(1:9) == "operator(") is_spec = .true.
        end if
        if (len(lowered) > 11) then
            if (lowered(1:11) == "assignment(") is_spec = .true.
        end if
    end function is_generic_spec

end module generic_spec_names
