module parser_prefix_buffer_module
    implicit none
    private

    character(len=16), allocatable, save :: global_prefixes(:)

    type, public :: parser_prefix_buffer_t
    contains
        procedure :: clear => prefix_buffer_clear
        procedure :: set => prefix_buffer_set
        procedure :: append_all => prefix_buffer_append_all
        procedure :: consume => prefix_buffer_consume
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
        if (allocated(global_prefixes)) then
            deallocate (global_prefixes)
        end if
        allocate (character(len=16) :: global_prefixes(0))
    end subroutine prefix_buffer_clear

    subroutine prefix_buffer_set(this, prefixes)
        class(parser_prefix_buffer_t), intent(inout) :: this
        character(len=16), allocatable, intent(in) :: prefixes(:)

        call ensure_initialized()
        call this%clear()
        if (allocated(prefixes)) then
            if (size(prefixes) > 0) then
                if (allocated(global_prefixes)) then
                    deallocate (global_prefixes)
                end if
                allocate (character(len=16) :: global_prefixes(size(prefixes)))
                global_prefixes = prefixes
            end if
        end if
    end subroutine prefix_buffer_set

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
