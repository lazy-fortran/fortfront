module parser_expression_stacks_module
    use lexer_core, only: token_t
    implicit none
    private

    integer, parameter :: MAX_OPERATOR_LEN = 16
    integer, parameter :: STACK_DEFAULT_CAPACITY = 32

    type :: operator_entry_t
        character(len=MAX_OPERATOR_LEN) :: symbol = ""
        integer :: precedence = 0
        logical :: right_associative = .false.
        logical :: is_group = .false.
        integer :: token_line = 1
        integer :: token_column = 1
    end type operator_entry_t

    type :: operator_stack_t
        type(operator_entry_t), allocatable :: values(:)
        integer :: size = 0
    end type operator_stack_t

    type :: operand_stack_t
        integer, allocatable :: values(:)
        integer :: size = 0
    end type operand_stack_t

    type :: token_stack_t
        type(token_t), allocatable :: values(:)
        integer :: size = 0
    end type token_stack_t

    public :: MAX_OPERATOR_LEN
    public :: operator_entry_t
    public :: operator_stack_t
    public :: operand_stack_t
    public :: token_stack_t
    public :: operator_stack_clear
    public :: operator_stack_ensure_capacity
    public :: operator_stack_push
    public :: operator_stack_pop
    public :: operator_stack_peek
    public :: operator_stack_is_empty
    public :: operator_stack_has_open_group
    public :: operand_stack_clear
    public :: operand_stack_ensure_capacity
    public :: operand_stack_push
    public :: operand_stack_pop
    public :: operand_stack_peek
    public :: operand_stack_is_empty
    public :: token_stack_clear
    public :: token_stack_ensure_capacity
    public :: token_stack_push
    public :: token_stack_pop
    public :: token_stack_is_empty

contains

    subroutine operator_stack_clear(stack)
        type(operator_stack_t), intent(inout) :: stack

        stack%size = 0
    end subroutine operator_stack_clear

    subroutine operator_stack_ensure_capacity(stack, desired)
        type(operator_stack_t), intent(inout) :: stack
        integer, intent(in) :: desired
        type(operator_entry_t), allocatable :: new_values(:)
        integer :: new_capacity

        if (.not. allocated(stack%values)) then
            allocate (stack%values(max(STACK_DEFAULT_CAPACITY, desired)))
            stack%size = 0
            return
        end if

        if (size(stack%values) >= desired) return

        new_capacity = max(size(stack%values) * 2, desired)
        allocate (new_values(new_capacity))
        if (stack%size > 0) then
            new_values(1:stack%size) = stack%values(1:stack%size)
        end if
        call move_alloc(new_values, stack%values)
    end subroutine operator_stack_ensure_capacity

    subroutine operator_stack_push(stack, entry)
        type(operator_stack_t), intent(inout) :: stack
        type(operator_entry_t), intent(in) :: entry

        call operator_stack_ensure_capacity(stack, stack%size + 1)
        stack%size = stack%size + 1
        stack%values(stack%size) = entry
    end subroutine operator_stack_push

    function operator_stack_pop(stack) result(entry)
        type(operator_stack_t), intent(inout) :: stack
        type(operator_entry_t) :: entry

        if (stack%size <= 0) then
            entry = operator_entry_t()
            return
        end if

        entry = stack%values(stack%size)
        stack%size = stack%size - 1
    end function operator_stack_pop

    function operator_stack_peek(stack) result(entry)
        type(operator_stack_t), intent(in) :: stack
        type(operator_entry_t) :: entry

        if (stack%size <= 0) then
            entry = operator_entry_t()
        else
            entry = stack%values(stack%size)
        end if
    end function operator_stack_peek

    logical function operator_stack_is_empty(stack)
        type(operator_stack_t), intent(in) :: stack
        operator_stack_is_empty = (stack%size <= 0)
    end function operator_stack_is_empty

    logical function operator_stack_has_open_group(stack)
        type(operator_stack_t), intent(in) :: stack
        integer :: idx

        operator_stack_has_open_group = .false.
        if (.not. allocated(stack%values)) return

        do idx = stack%size, 1, -1
            if (stack%values(idx)%is_group) then
                operator_stack_has_open_group = .true.
                return
            end if
        end do
    end function operator_stack_has_open_group

    subroutine operand_stack_clear(stack)
        type(operand_stack_t), intent(inout) :: stack

        stack%size = 0
    end subroutine operand_stack_clear

    subroutine operand_stack_ensure_capacity(stack, desired)
        type(operand_stack_t), intent(inout) :: stack
        integer, intent(in) :: desired
        integer, allocatable :: new_values(:)
        integer :: new_capacity

        if (.not. allocated(stack%values)) then
            allocate (stack%values(max(STACK_DEFAULT_CAPACITY, desired)))
            stack%size = 0
            return
        end if

        if (size(stack%values) >= desired) return

        new_capacity = max(size(stack%values) * 2, desired)
        allocate (new_values(new_capacity))
        if (stack%size > 0) then
            new_values(1:stack%size) = stack%values(1:stack%size)
        end if
        call move_alloc(new_values, stack%values)
    end subroutine operand_stack_ensure_capacity

    subroutine operand_stack_push(stack, value)
        type(operand_stack_t), intent(inout) :: stack
        integer, intent(in) :: value

        call operand_stack_ensure_capacity(stack, stack%size + 1)
        stack%size = stack%size + 1
        stack%values(stack%size) = value
    end subroutine operand_stack_push

    integer function operand_stack_pop(stack)
        type(operand_stack_t), intent(inout) :: stack

        if (stack%size <= 0) then
            operand_stack_pop = 0
            return
        end if

        operand_stack_pop = stack%values(stack%size)
        stack%size = stack%size - 1
    end function operand_stack_pop

    integer function operand_stack_peek(stack)
        type(operand_stack_t), intent(in) :: stack

        if (stack%size <= 0) then
            operand_stack_peek = 0
        else
            operand_stack_peek = stack%values(stack%size)
        end if
    end function operand_stack_peek

    logical function operand_stack_is_empty(stack)
        type(operand_stack_t), intent(in) :: stack
        operand_stack_is_empty = (stack%size <= 0)
    end function operand_stack_is_empty

    subroutine token_stack_clear(stack)
        type(token_stack_t), intent(inout) :: stack

        stack%size = 0
    end subroutine token_stack_clear

    subroutine token_stack_ensure_capacity(stack, desired)
        type(token_stack_t), intent(inout) :: stack
        integer, intent(in) :: desired
        type(token_t), allocatable :: new_values(:)
        integer :: new_capacity

        if (.not. allocated(stack%values)) then
            allocate (stack%values(max(STACK_DEFAULT_CAPACITY, desired)))
            stack%size = 0
            return
        end if

        if (size(stack%values) >= desired) return

        new_capacity = max(size(stack%values) * 2, desired)
        allocate (new_values(new_capacity))
        if (stack%size > 0) then
            new_values(1:stack%size) = stack%values(1:stack%size)
        end if
        call move_alloc(new_values, stack%values)
    end subroutine token_stack_ensure_capacity

    subroutine token_stack_push(stack, value)
        type(token_stack_t), intent(inout) :: stack
        type(token_t), intent(in) :: value

        call token_stack_ensure_capacity(stack, stack%size + 1)
        stack%size = stack%size + 1
        stack%values(stack%size) = value
    end subroutine token_stack_push

    function token_stack_pop(stack) result(token)
        type(token_stack_t), intent(inout) :: stack
        type(token_t) :: token

        if (stack%size <= 0) then
            token = token_t()
            return
        end if

        token = stack%values(stack%size)
        stack%size = stack%size - 1
    end function token_stack_pop

    logical function token_stack_is_empty(stack)
        type(token_stack_t), intent(in) :: stack
        token_stack_is_empty = (stack%size <= 0)
    end function token_stack_is_empty

end module parser_expression_stacks_module
