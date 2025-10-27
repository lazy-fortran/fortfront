! Test intent(out) array shape inference with parameter-determined dimensions
program test_issue_1621_intent_out_shape_inference
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg
    logical :: has_n1_decl, has_n2_decl

    source = 'module m_intent_out' // new_line('a') // &
             '' // new_line('a') // &
             '  implicit none' // new_line('a') // &
             '  public' // new_line('a') // &
             '' // new_line('a') // &
             'contains' // new_line('a') // &
             '' // new_line('a') // &
             '  subroutine interpolation(n1,n2,a1,a2,output)' // new_line('a') // &
             '    !' // new_line('a') // &
             '    integer,               intent(in)    :: n1,n2' // new_line('a') // &
             '    real,dimension(n1),    intent(in)    :: a1' // new_line('a') // &
             '    real,dimension(n2),    intent(in)    :: a2' // new_line('a') // &
             '    real,dimension(n1,n2), intent(out)   :: output' // new_line('a') // &
             '' // new_line('a') // &
             '    integer :: i,j' // new_line('a') // &
             '' // new_line('a') // &
             '    do j=1,n2' // new_line('a') // &
             '      do i=1,n1' // new_line('a') // &
             '         output(i,j)=(a1(i)+a2(j))/2' // new_line('a') // &
             '      enddo' // new_line('a') // &
             '    enddo' // new_line('a') // &
             '' // new_line('a') // &
             '  end subroutine interpolation' // new_line('a') // &
             '' // new_line('a') // &
             'end module m_intent_out'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    has_n1_decl = (index(transformed, 'integer :: n1') > 0) .or. &
                  (index(transformed, 'integer :: n1,') > 0) .or. &
                  (index(transformed, 'integer, intent(in) :: n1') > 0) .or. &
                  (index(transformed, 'integer, intent(in) :: n1,') > 0) .or. &
                  (index(transformed, ':: n1, n2') > 0) .or. &
                  (index(transformed, ':: n1,n2') > 0)

    has_n2_decl = (index(transformed, 'integer :: n2') > 0) .or. &
                  (index(transformed, 'integer :: n1, n2') > 0) .or. &
                  (index(transformed, 'integer :: n1,n2') > 0) .or. &
                  (index(transformed, 'integer, intent(in) :: n2') > 0) .or. &
                  (index(transformed, 'integer, intent(in) :: n1, n2') > 0) .or. &
                  (index(transformed, 'integer, intent(in) :: n1,n2') > 0)

    if (.not. (has_n1_decl .and. has_n2_decl)) then
        print *, 'FAIL: parameter declarations n1,n2 missing'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if ((index(transformed, 'dimension(n1)') == 0 .and. &
         index(transformed, 'a1(n1)') == 0) .or. &
        (index(transformed, 'dimension(n2)') == 0 .and. &
         index(transformed, 'a2(n2)') == 0)) then
        print *, 'FAIL: dimension specifications missing for input arrays a1,a2'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'dimension(n1, n2)') == 0 .and. &
        index(transformed, 'dimension(n1,n2)') == 0 .and. &
        index(transformed, 'output(n1, n2)') == 0 .and. &
        index(transformed, 'output(n1,n2)') == 0) then
        print *, 'FAIL: intent(out) array output with shape (n1,n2) missing'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'output(i, j)') == 0 .and. &
        index(transformed, 'output(i,j)') == 0) then
        print *, 'FAIL: array indexing in loop body missing'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'subroutine interpolation(') == 0) then
        print *, 'FAIL: subroutine declaration missing'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    print *, 'PASS: intent(out) array shape inference preserved'
end program test_issue_1621_intent_out_shape_inference
