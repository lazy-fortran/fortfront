module parser_prefix_state
   implicit none
   private

   character(len=16), allocatable :: stored_prefixes(:)

   public :: set_pending_prefixes, append_pending_prefixes, consume_pending_prefixes
   public :: has_pending_prefixes, clear_pending_prefixes, get_pending_prefixes, append_prefix_token

contains

   subroutine clear_pending_prefixes()
      if (allocated(stored_prefixes)) deallocate (stored_prefixes)
   end subroutine clear_pending_prefixes

   subroutine set_pending_prefixes(prefixes)
      character(len=16), allocatable, intent(in) :: prefixes(:)

      call clear_pending_prefixes()
      if (allocated(prefixes)) then
         if (size(prefixes) > 0) then
            allocate (character(len=16) :: stored_prefixes(size(prefixes)))
            stored_prefixes = prefixes
         end if
      end if
   end subroutine set_pending_prefixes

   subroutine append_pending_prefixes(prefixes)
      character(len=16), allocatable, intent(in) :: prefixes(:)
      integer :: i

      if (.not. allocated(prefixes)) return
      if (size(prefixes) == 0) return

      if (.not. allocated(stored_prefixes)) then
         allocate (character(len=16) :: stored_prefixes(0))
      end if

      do i = 1, size(prefixes)
         call append_prefix_token(stored_prefixes, prefixes(i))
      end do
   end subroutine append_pending_prefixes

   subroutine get_pending_prefixes(prefixes)
      character(len=16), allocatable, intent(out) :: prefixes(:)
      if (allocated(stored_prefixes)) then
         allocate (character(len=16) :: prefixes(size(stored_prefixes)))
         prefixes = stored_prefixes
      else
         allocate (character(len=16) :: prefixes(0))
      end if
   end subroutine get_pending_prefixes

   subroutine consume_pending_prefixes(prefixes)
      character(len=16), allocatable, intent(out) :: prefixes(:)
      call get_pending_prefixes(prefixes)
      call clear_pending_prefixes()
   end subroutine consume_pending_prefixes

   logical function has_pending_prefixes()
      has_pending_prefixes = allocated(stored_prefixes) .and. size(stored_prefixes) > 0
   end function has_pending_prefixes

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

end module parser_prefix_state
