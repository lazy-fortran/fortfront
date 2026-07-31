module source_bom
    ! Byte-order-mark handling for source text (issue #2943).
    !
    ! gfortran accepts a source file that starts with a byte-order mark
    ! (gfortran.dg/bom_*.f90), so the leading BOM must be removed before any
    ! character validation or tokenization. A UTF-8 BOM is simply stripped; a
    ! UTF-16/UTF-32 BOM additionally selects the wide decoding of the ASCII
    ! payload. Text without a BOM is returned unchanged, so bytes that are not
    ! part of a leading BOM stay subject to the normal source-character rules.
    implicit none
    private

    public :: decode_source_bom

contains

    pure subroutine decode_source_bom(raw_text, decoded)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded
        logical :: ok

        call try_strip_utf8(raw_text, decoded, ok)
        if (ok) return

        call try_decode_utf32_be(raw_text, decoded, ok)
        if (ok) return

        call try_decode_utf32_le(raw_text, decoded, ok)
        if (ok) return

        call try_decode_utf16_be(raw_text, decoded, ok)
        if (ok) return

        call try_decode_utf16_le(raw_text, decoded, ok)
        if (ok) return

        allocate (character(len=len(raw_text)) :: decoded)
        decoded = raw_text
    end subroutine decode_source_bom

    pure subroutine try_strip_utf8(raw_text, decoded, ok)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded
        logical, intent(out) :: ok
        integer :: n

        ok = .false.
        n = len(raw_text)
        if (n < 3) return
        if (iachar(raw_text(1:1)) /= 239) return
        if (iachar(raw_text(2:2)) /= 187) return
        if (iachar(raw_text(3:3)) /= 191) return

        allocate (character(len=n - 3) :: decoded)
        decoded = raw_text(4:n)
        ok = .true.
    end subroutine try_strip_utf8

    pure subroutine try_decode_utf16_le(raw_text, decoded, ok)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded
        logical, intent(out) :: ok
        integer :: n, out_len, i, out_idx
        integer :: b_lo, b_hi

        ok = .false.
        n = len(raw_text)
        if (n < 4) return
        if (iachar(raw_text(1:1)) /= 255) return
        if (iachar(raw_text(2:2)) /= 254) return
        if (mod(n - 2, 2) /= 0) return

        out_len = (n - 2)/2
        allocate (character(len=out_len) :: decoded)
        out_idx = 0
        i = 3
        do while (i <= n)
            b_lo = iachar(raw_text(i:i))
            b_hi = iachar(raw_text(i + 1:i + 1))
            if (b_hi /= 0) then
                deallocate (decoded)
                return
            end if
            out_idx = out_idx + 1
            decoded(out_idx:out_idx) = char(b_lo)
            i = i + 2
        end do
        ok = .true.
    end subroutine try_decode_utf16_le

    pure subroutine try_decode_utf16_be(raw_text, decoded, ok)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded
        logical, intent(out) :: ok
        integer :: n, out_len, i, out_idx
        integer :: b_hi, b_lo

        ok = .false.
        n = len(raw_text)
        if (n < 4) return
        if (iachar(raw_text(1:1)) /= 254) return
        if (iachar(raw_text(2:2)) /= 255) return
        if (mod(n - 2, 2) /= 0) return

        out_len = (n - 2)/2
        allocate (character(len=out_len) :: decoded)
        out_idx = 0
        i = 3
        do while (i <= n)
            b_hi = iachar(raw_text(i:i))
            b_lo = iachar(raw_text(i + 1:i + 1))
            if (b_hi /= 0) then
                deallocate (decoded)
                return
            end if
            out_idx = out_idx + 1
            decoded(out_idx:out_idx) = char(b_lo)
            i = i + 2
        end do
        ok = .true.
    end subroutine try_decode_utf16_be

    pure subroutine try_decode_utf32_le(raw_text, decoded, ok)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded
        logical, intent(out) :: ok
        integer :: n, out_len, i, out_idx
        integer :: b1, b2, b3, b4

        ok = .false.
        n = len(raw_text)
        if (n < 8) return
        b1 = iachar(raw_text(1:1))
        b2 = iachar(raw_text(2:2))
        b3 = iachar(raw_text(3:3))
        b4 = iachar(raw_text(4:4))
        if (b1 /= 255 .or. b2 /= 254) return
        if (b3 /= 0 .or. b4 /= 0) return
        if (mod(n - 4, 4) /= 0) return

        out_len = (n - 4)/4
        allocate (character(len=out_len) :: decoded)
        out_idx = 0
        i = 5
        do while (i + 3 <= n)
            b1 = iachar(raw_text(i:i))
            b2 = iachar(raw_text(i + 1:i + 1))
            b3 = iachar(raw_text(i + 2:i + 2))
            b4 = iachar(raw_text(i + 3:i + 3))
            if (b2 /= 0 .or. b3 /= 0 .or. b4 /= 0) then
                deallocate (decoded)
                return
            end if
            out_idx = out_idx + 1
            decoded(out_idx:out_idx) = char(b1)
            i = i + 4
        end do
        ok = .true.
    end subroutine try_decode_utf32_le

    pure subroutine try_decode_utf32_be(raw_text, decoded, ok)
        character(len=*), intent(in) :: raw_text
        character(len=:), allocatable, intent(out) :: decoded
        logical, intent(out) :: ok
        integer :: n, out_len, i, out_idx
        integer :: b1, b2, b3, b4

        ok = .false.
        n = len(raw_text)
        if (n < 8) return
        b1 = iachar(raw_text(1:1))
        b2 = iachar(raw_text(2:2))
        b3 = iachar(raw_text(3:3))
        b4 = iachar(raw_text(4:4))
        if (b1 /= 0 .or. b2 /= 0) return
        if (b3 /= 254 .or. b4 /= 255) return
        if (mod(n - 4, 4) /= 0) return

        out_len = (n - 4)/4
        allocate (character(len=out_len) :: decoded)
        out_idx = 0
        i = 5
        do while (i + 3 <= n)
            b1 = iachar(raw_text(i:i))
            b2 = iachar(raw_text(i + 1:i + 1))
            b3 = iachar(raw_text(i + 2:i + 2))
            b4 = iachar(raw_text(i + 3:i + 3))
            if (b1 /= 0 .or. b2 /= 0 .or. b3 /= 0) then
                deallocate (decoded)
                return
            end if
            out_idx = out_idx + 1
            decoded(out_idx:out_idx) = char(b4)
            i = i + 4
        end do
        ok = .true.
    end subroutine try_decode_utf32_be

end module source_bom
