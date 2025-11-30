program test_bom_handling
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    call test_utf16_le_bom()
    call test_utf16_be_bom()
    call test_utf32_le_bom()
    call test_utf32_be_bom()

    print *, 'PASS: BOM-encoded inputs accepted'

contains

    subroutine test_utf16_le_bom()
        character(len=:), allocatable :: ascii_src
        character(len=:), allocatable :: bom_src
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        ascii_src = 'x = 5' // new_line('A')
        call make_utf16_le_bom(ascii_src, bom_src)

        call transform_lazy_fortran_string(bom_src, output, error_msg)
        call assert_no_binary_error('UTF-16 LE', error_msg)
    end subroutine test_utf16_le_bom

    subroutine test_utf16_be_bom()
        character(len=:), allocatable :: ascii_src
        character(len=:), allocatable :: bom_src
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        ascii_src = 'x = 5' // new_line('A')
        call make_utf16_be_bom(ascii_src, bom_src)

        call transform_lazy_fortran_string(bom_src, output, error_msg)
        call assert_no_binary_error('UTF-16 BE', error_msg)
    end subroutine test_utf16_be_bom

    subroutine test_utf32_le_bom()
        character(len=:), allocatable :: ascii_src
        character(len=:), allocatable :: bom_src
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        ascii_src = 'x = 5' // new_line('A')
        call make_utf32_le_bom(ascii_src, bom_src)

        call transform_lazy_fortran_string(bom_src, output, error_msg)
        call assert_no_binary_error('UTF-32 LE', error_msg)
    end subroutine test_utf32_le_bom

    subroutine test_utf32_be_bom()
        character(len=:), allocatable :: ascii_src
        character(len=:), allocatable :: bom_src
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        ascii_src = 'x = 5' // new_line('A')
        call make_utf32_be_bom(ascii_src, bom_src)

        call transform_lazy_fortran_string(bom_src, output, error_msg)
        call assert_no_binary_error('UTF-32 BE', error_msg)
    end subroutine test_utf32_be_bom

    subroutine assert_no_binary_error(label, error_msg)
        character(len=*), intent(in) :: label
        character(len=:), allocatable, intent(in) :: error_msg

        if (allocated(error_msg)) then
            if (index(error_msg, 'Input appears to be binary data') /= 0) then
                write (error_unit, '(A,A)') &
                    'FAIL: BOM input classified as binary for ', trim(label)
                write (error_unit, '(A)') trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine assert_no_binary_error

    subroutine make_utf16_le_bom(ascii_src, bom_src)
        character(len=*), intent(in) :: ascii_src
        character(len=:), allocatable, intent(out) :: bom_src
        integer :: n, i, out_len, pos

        n = len(ascii_src)
        out_len = 2 + 2 * n
        allocate (character(len=out_len) :: bom_src)

        bom_src(1:1) = char(255)
        bom_src(2:2) = char(254)

        pos = 3
        do i = 1, n
            bom_src(pos:pos) = ascii_src(i:i)
            bom_src(pos + 1:pos + 1) = char(0)
            pos = pos + 2
        end do
    end subroutine make_utf16_le_bom

    subroutine make_utf16_be_bom(ascii_src, bom_src)
        character(len=*), intent(in) :: ascii_src
        character(len=:), allocatable, intent(out) :: bom_src
        integer :: n, i, out_len, pos

        n = len(ascii_src)
        out_len = 2 + 2 * n
        allocate (character(len=out_len) :: bom_src)

        bom_src(1:1) = char(254)
        bom_src(2:2) = char(255)

        pos = 3
        do i = 1, n
            bom_src(pos:pos) = char(0)
            bom_src(pos + 1:pos + 1) = ascii_src(i:i)
            pos = pos + 2
        end do
    end subroutine make_utf16_be_bom

    subroutine make_utf32_le_bom(ascii_src, bom_src)
        character(len=*), intent(in) :: ascii_src
        character(len=:), allocatable, intent(out) :: bom_src
        integer :: n, i, out_len, pos

        n = len(ascii_src)
        out_len = 4 + 4 * n
        allocate (character(len=out_len) :: bom_src)

        bom_src(1:1) = char(255)
        bom_src(2:2) = char(254)
        bom_src(3:3) = char(0)
        bom_src(4:4) = char(0)

        pos = 5
        do i = 1, n
            bom_src(pos:pos) = ascii_src(i:i)
            bom_src(pos + 1:pos + 1) = char(0)
            bom_src(pos + 2:pos + 2) = char(0)
            bom_src(pos + 3:pos + 3) = char(0)
            pos = pos + 4
        end do
    end subroutine make_utf32_le_bom

    subroutine make_utf32_be_bom(ascii_src, bom_src)
        character(len=*), intent(in) :: ascii_src
        character(len=:), allocatable, intent(out) :: bom_src
        integer :: n, i, out_len, pos

        n = len(ascii_src)
        out_len = 4 + 4 * n
        allocate (character(len=out_len) :: bom_src)

        bom_src(1:1) = char(0)
        bom_src(2:2) = char(0)
        bom_src(3:3) = char(254)
        bom_src(4:4) = char(255)

        pos = 5
        do i = 1, n
            bom_src(pos:pos) = char(0)
            bom_src(pos + 1:pos + 1) = char(0)
            bom_src(pos + 2:pos + 2) = char(0)
            bom_src(pos + 3:pos + 3) = ascii_src(i:i)
            pos = pos + 4
        end do
    end subroutine make_utf32_be_bom

end program test_bom_handling

