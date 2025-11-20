C Issue #2391: fixed form allocation with continuation lines
      program fixed_form_allocation_bounds
      implicit none
      integer :: nf10
      logical, allocatable :: lla(:,:,:)

      nf10 = 10
      allocate (lla(2:3, 4,
     $              nf10:1,
     $              -2:7))

      end program fixed_form_allocation_bounds
