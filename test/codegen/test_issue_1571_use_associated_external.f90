program test_issue_1571_use_associated_external
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    implicit none
    logical :: passed

    passed = run_use_associated_external_case()
    if (passed) then
        write (*, '(A)') &
            'PASS: use-associated functions avoid external declarations'
        stop 0
    end if

    write (error_unit, '(A)') &
        'FAIL: use-associated functions injected external declarations'
    stop 1

contains

    function run_use_associated_external_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        call read_example('examples/f90/issue_1571_use_associated_external.f90', source)

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.
        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'external :: add_one') > 0) then
            write (error_unit, '(A)') &
                'unexpected external declaration for add_one'
            passed = .false.
        end if

        if (index(generated, 'add_one(arr)') == 0) then
            write (error_unit, '(A)') 'missing add_one call in output'
            passed = .false.
        end if
    end function run_use_associated_external_case

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=stat) buffer
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_1571_use_associated_external
