module parser_prefix_buffer_module
    implicit none
    private

    character(len=16), allocatable, save :: global_prefixes(:)
    character(len=:), allocatable, save :: global_return_type

    type, public :: parser_prefix_buffer_t
    contains
        procedure :: clear => prefix_buffer_clear
        procedure :: set => prefix_buffer_set
        procedure :: set_return_type => prefix_buffer_set_return_type
        procedure :: append_all => prefix_buffer_append_all
        procedure :: consume => prefix_buffer_consume
        procedure :: consume_return_type => prefix_buffer_consume_return_type
        procedure :: has_pending => prefix_buffer_has
        procedure :: get_all => prefix_buffer_get
    end type parser_prefix_buffer_t

    public :: append_prefix_token

contains

    subroutine ensure_initialized()
        if (.not. allocated(global_prefixes)) then
            allocate (character(len=16) :: global_prefixes(0))
        end if
    end subroutine ensure_initialized

    subroutine prefix_buffer_clear(this)
        class(parser_prefix_buffer_t), intent(inout) :: this

        call ensure_initialized()
        if (allocated(global_return_type)) deallocate (global_return_type)
        block
            character(len=16), allocatable :: empty(:)
            allocate (character(len=16) :: empty(0))
            call move_alloc(empty, global_prefixes)
        end block
    end subroutine prefix_buffer_clear

    subroutine prefix_buffer_set(this, prefixes)
        class(parser_prefix_buffer_t), intent(inout) :: this
        character(len=16), allocatable, intent(in) :: prefixes(:)

        call ensure_initialized()
        call this%clear()
        if (allocated(prefixes)) then
            if (size(prefixes) > 0) then
                block
                    character(len=16), allocatable :: new_prefixes(:)
                    allocate (character(len=16) :: new_prefixes(size(prefixes)))
                    new_prefixes = prefixes
                    call move_alloc(new_prefixes, global_prefixes)
                end block
            end if
        end if
    end subroutine prefix_buffer_set

    subroutine prefix_buffer_set_return_type(this, return_type)
        class(parser_prefix_buffer_t), intent(inout) :: this
        character(len=*), intent(in) :: return_type

        if (allocated(global_return_type)) deallocate (global_return_type)
        if (len_trim(return_type) > 0) global_return_type = trim(return_type)
    end subroutine prefix_buffer_set_return_type

    subroutine prefix_buffer_append_all(this, prefixes)
        class(parser_prefix_buffer_t), intent(inout) :: this
        character(len=16), allocatable, intent(in) :: prefixes(:)
        integer :: i

        call ensure_initialized()
        if (.not. allocated(prefixes)) return
        if (size(prefixes) == 0) return

        do i = 1, size(prefixes)
            call append_prefix_token(global_prefixes, prefixes(i))
        end do
    end subroutine prefix_buffer_append_all

    subroutine prefix_buffer_get(this, prefixes)
        class(parser_prefix_buffer_t), intent(in) :: this
        character(len=16), allocatable, intent(out) :: prefixes(:)

        call ensure_initialized()
        if (size(global_prefixes) > 0) then
            allocate (character(len=16) :: prefixes(size(global_prefixes)))
            prefixes = global_prefixes
        else
            allocate (character(len=16) :: prefixes(0))
        end if
    end subroutine prefix_buffer_get

    subroutine prefix_buffer_consume(this, prefixes)
        class(parser_prefix_buffer_t), intent(inout) :: this
        character(len=16), allocatable, intent(out) :: prefixes(:)

        call this%get_all(prefixes)
        call this%clear()
    end subroutine prefix_buffer_consume

    subroutine prefix_buffer_consume_return_type(this, return_type)
        class(parser_prefix_buffer_t), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: return_type

        return_type = ''
        if (allocated(global_return_type)) then
            return_type = global_return_type
            deallocate (global_return_type)
        end if
    end subroutine prefix_buffer_consume_return_type

    logical function prefix_buffer_has(this)
        class(parser_prefix_buffer_t), intent(in) :: this

        call ensure_initialized()
        prefix_buffer_has = size(global_prefixes) > 0
    end function prefix_buffer_has

    subroutine append_prefix_token(array, value)
        character(len=16), allocatable, intent(inout) :: array(:)
        character(len=*), intent(in) :: value
        integer :: n, i
        character(len=16), allocatable :: temp(:)
        logical :: exists

        exists = .false.
        if (allocated(array)) then
            do i = 1, size(array)
                if (trim(array(i)) == trim(value)) then
                    exists = .true.
                    exit
                end if
            end do
        else
            allocate (character(len=16) :: array(0))
        end if

        if (exists) return

        n = size(array)
        allocate (character(len=16) :: temp(n + 1))
        if (n > 0) temp(1:n) = array
        temp(n + 1) = trim(value)
        call move_alloc(temp, array)
    end subroutine append_prefix_token

end module parser_prefix_buffer_module
