module cst_core
    use, intrinsic :: iso_fortran_env, only: int64
    use cst_nodes
    use cst_arena
    implicit none
    private

    public :: create_cst_node, create_trivia
    public :: get_node_kind_name, is_trivia_kind
    public :: validate_cst_node, validate_trivia

contains

    ! Create a new CST node
    function create_cst_node(kind, start_pos, end_pos) result(node)
        integer, intent(in) :: kind
        integer, intent(in) :: start_pos
        integer, intent(in) :: end_pos
        type(cst_node_t) :: node
        
        node%kind = kind
        node%uid = 0_int64  ! Will be set by arena
        node%start_pos = start_pos
        node%end_pos = end_pos
        ! Trivia arrays are unallocated initially
    end function create_cst_node

    ! Create trivia (comments, whitespace, newlines)
    function create_trivia(kind, text, start_pos, end_pos) result(trivia)
        integer, intent(in) :: kind
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos
        integer, intent(in) :: end_pos
        type(trivia_t) :: trivia
        
        trivia%kind = kind
        trivia%text = text
        trivia%start_pos = start_pos
        trivia%end_pos = end_pos
    end function create_trivia

    ! Get human-readable name for node kind
    function get_node_kind_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=:), allocatable :: name
        
        select case (kind)
        case (CST_PROGRAM)
            name = "PROGRAM"
        case (CST_SUBROUTINE)
            name = "SUBROUTINE"
        case (CST_FUNCTION)
            name = "FUNCTION"
        case (CST_DECLARATION)
            name = "DECLARATION"
        case (CST_ASSIGNMENT)
            name = "ASSIGNMENT"
        case (CST_CALL)
            name = "CALL"
        case (CST_IDENTIFIER)
            name = "IDENTIFIER"
        case (CST_LITERAL)
            name = "LITERAL"
        case (CST_OPERATOR)
            name = "OPERATOR"
        case (CST_COMMENT)
            name = "COMMENT"
        case (CST_WHITESPACE)
            name = "WHITESPACE"
        case (CST_NEWLINE)
            name = "NEWLINE"
        case default
            name = "UNKNOWN"
        end select
    end function get_node_kind_name

    ! Check if kind represents trivia
    function is_trivia_kind(kind) result(is_trivia)
        integer, intent(in) :: kind
        logical :: is_trivia
        
        is_trivia = (kind == CST_COMMENT .or. &
                     kind == CST_WHITESPACE .or. &
                     kind == CST_NEWLINE)
    end function is_trivia_kind

    ! Validate CST node structure
    function validate_cst_node(node) result(is_valid)
        type(cst_node_t), intent(in) :: node
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check basic properties
        if (node%kind < CST_PROGRAM .or. node%kind > CST_NEWLINE) then
            is_valid = .false.
            return
        end if
        
        if (node%start_pos < 0 .or. node%end_pos < node%start_pos) then
            is_valid = .false.
            return
        end if
        
        ! Validate trivia if present
        if (allocated(node%leading_trivia)) then
            if (.not. all_trivia_valid(node%leading_trivia)) then
                is_valid = .false.
                return
            end if
        end if
        
        if (allocated(node%trailing_trivia)) then
            if (.not. all_trivia_valid(node%trailing_trivia)) then
                is_valid = .false.
                return
            end if
        end if
    end function validate_cst_node

    ! Validate trivia structure
    function validate_trivia(trivia) result(is_valid)
        type(trivia_t), intent(in) :: trivia
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check kind is trivia
        if (.not. is_trivia_kind(trivia%kind)) then
            is_valid = .false.
            return
        end if
        
        ! Check positions
        if (trivia%start_pos < 0 .or. trivia%end_pos < trivia%start_pos) then
            is_valid = .false.
            return
        end if
        
        ! Check text is allocated
        if (.not. allocated(trivia%text)) then
            is_valid = .false.
            return
        end if
    end function validate_trivia

    ! Helper: validate all trivia in array
    function all_trivia_valid(trivia_array) result(all_valid)
        type(trivia_t), intent(in) :: trivia_array(:)
        logical :: all_valid
        
        integer :: i
        
        all_valid = .true.
        do i = 1, size(trivia_array)
            if (.not. validate_trivia(trivia_array(i))) then
                all_valid = .false.
                return
            end if
        end do
    end function all_trivia_valid

end module cst_core