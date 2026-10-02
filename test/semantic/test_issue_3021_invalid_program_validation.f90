program test_issue_3021_invalid_program_validation
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none
    character(len=1), parameter :: nl = new_line('a')
    character(len=*), parameter :: types = &
        'module types'//nl//'type shared'//nl//'integer n'//nl// &
        'end type'//nl//'end module'//nl

    call check('duplicate declaration', program_source( &
        'integer x'//nl//'integer x'), .false.)
    call check('duplicate list entity', program_source('integer x, x'), .false.)
    call check('use type variable collision', types//program_source( &
        'use types'//nl//'integer shared'), .false.)
    call check('renamed use type collision', types//program_source( &
        'use types, only: local => shared'//nl//'integer local'), .false.)
    call check('common member type collision', types//program_source( &
        'use types'//nl//'common /grp/ shared'), .false.)
    call check('loop variable type collision', types//program_source( &
        'use types'//nl//'do shared=1,2'//nl//'end do'), .false.)
    call check('parameter assignment', program_source( &
        'integer, parameter :: k=3'//nl//'k=4'), .false.)
    call check('parameter element assignment', program_source( &
        'integer, parameter :: k(2)=[1,2]'//nl//'k(1)=4'), .false.)
    call check('parameter loop control', program_source( &
        'integer, parameter :: k=3'//nl//'do k=1,2'//nl//'end do'), .false.)
    call check('len too many arguments', program_source( &
        'print *, len("ab",3,4)'), .false.)
    call check('len zero arguments', program_source('print *, len()'), .false.)
    call check('len_trim too many arguments', program_source( &
        'print *, len_trim("ab",3,4)'), .false.)
    call check('counted loop wrong name', program_source( &
        'integer i'//nl//'outer: do i=1,2'//nl//'end do inner'), .false.)
    call check('while loop wrong name', program_source( &
        'outer: do while (.false.)'//nl//'end do inner'), .false.)
    call check('enddo wrong name', program_source( &
        'integer i'//nl//'outer: do i=1,2'//nl//'enddo inner'), .false.)
    call check('unnamed loop ending name', program_source( &
        'integer i'//nl//'do i=1,2'//nl//'end do outer'), .false.)
    call check('named loop missing end name', program_source( &
        'integer i'//nl//'outer: do i=1,2'//nl//'end do'), .false.)
    call check('end program wrong name', 'program p'//nl// &
        'end program q', .false.)

    call check('ordinary local variable', program_source('integer x'), .true.)
    call check('dimension before type', program_source( &
        'dimension arr(3)'//nl//'double precision arr'//nl// &
        'arr(1)=1.5d0'), .true.)
    call check('dimension after type', program_source( &
        'integer arr'//nl//'dimension arr(3)'//nl//'arr(1)=1'), .true.)
    call check('external after type', program_source( &
        'integer e'//nl//'external e'), .true.)
    call check('external before type', program_source( &
        'external e'//nl//'integer e'), .true.)
    call check('typeless procedure attribute', program_source( &
        'integer e'//nl//'procedure() :: e'), .true.)
    call check('block variable shadow', program_source( &
        'integer x'//nl//'block'//nl//'integer x'//nl//'x=2'//nl// &
        'end block'//nl//'x=1'), .true.)
    call check('parameter block shadow', program_source( &
        'integer, parameter :: k=3'//nl//'block'//nl//'integer k'//nl// &
        'k=4'//nl//'end block'), .true.)
    call check('parameter keyword argument', program_source( &
        'integer, parameter :: kind=4'//nl//'print *, len("a", kind=kind)'), .true.)
    call check('type rename distinct variable', types//program_source( &
        'use types, only: local => shared'//nl//'integer shared'), .true.)
    call check('type imported elsewhere', types//program_source( &
        'integer shared'), .true.)
    call check('counted loop matching case', program_source( &
        'integer i'//nl//'outer: do i=1,2'//nl//'end do OUTER'), .true.)
    call check('while loop matching name', program_source( &
        'outer: do while (.false.)'//nl//'end do outer'), .true.)
    call check('len optional kind', program_source('print *, len("ab",4)'), .true.)
    call check('array named len', program_source( &
        'integer len(3)'//nl//'len(3)=2'), .true.)
    call check('procedure named len', 'module m'//nl//'contains'//nl// &
        'integer function len(a,b,c)'//nl//'integer a,b,c'//nl// &
        'len=a+b+c'//nl//'end function'//nl//'end module'//nl// &
        program_source('use m'//nl//'print *, len(1,2,3)'), .true.)
    call check('host parameter dummy shadow', 'module m'//nl// &
        'integer, parameter :: k=3'//nl//'contains'//nl//'subroutine s(k)'//nl// &
        'integer k'//nl//'k=4'//nl//'end subroutine'//nl//'end module', .true.)

    print *, 'PASS: invalid-program validation and legal shadowing'

contains

    function program_source(body) result(source)
        character(len=*), intent(in) :: body
        character(len=:), allocatable :: source

        source = 'program p'//nl//body//nl//'end program p'//nl
    end function program_source

    subroutine check(label, source, accepted)
        character(len=*), intent(in) :: label, source
        logical, intent(in) :: accepted
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(source, result, options)
        if (result%success() .eqv. accepted) return
        write (error_unit, '(a)') 'FAIL: '//label
        if (allocated(result%diagnostic_text)) then
            write (error_unit, '(a)') result%diagnostic_text
        end if
        stop 1
    end subroutine check

end program test_issue_3021_invalid_program_validation
