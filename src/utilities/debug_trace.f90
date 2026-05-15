module debug_trace
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, int64
    use cli_env, only: is_truthy
    use fortfront_constants, only: MAX_DEBUG_TRACE_FILE_NAME_LEN
    implicit none
    private

    integer, save :: depth = 0
    logical, save :: enabled = .false.
    logical, save :: initialized = .false.
    integer, parameter :: MAX_DEPTH = 2000
    integer, save :: file_u = -1
    character(len=MAX_DEBUG_TRACE_FILE_NAME_LEN), save :: file_name = ''
    logical, save :: profile_enabled = .false.
    integer(int64), save :: profile_clock_rate = 0_int64

    type :: profile_section_t
        character(len=:), allocatable :: name
        integer(int64) :: call_count = 0_int64
        integer(int64) :: total_counts = 0_int64
        integer(int64) :: self_counts = 0_int64
    end type profile_section_t

    type :: profile_frame_t
        integer :: section_index = 0
        integer(int64) :: start_count = 0_int64
        integer(int64) :: child_counts = 0_int64
    end type profile_frame_t

    type(profile_section_t), allocatable, save :: profile_sections(:)
    type(profile_frame_t), allocatable, save :: profile_stack(:)
    integer, save :: profile_depth = 0
    integer(int64), save :: profile_root_total_counts = 0_int64

    public :: trace_finalize
    public :: trace_init, trace_enter, trace_leave, trace_is_enabled
    public :: trace_profile_get_stat
    public :: trace_profile_reset
    public :: trace_set_profile_enabled

contains

    subroutine trace_init()
        character(len=64) :: val
        integer :: stat
        integer :: rate_default
        if (initialized) return
        val = ''
        call get_environment_variable('FORTFRONT_TRACE', val, status=stat)
        if (stat == 0) then
            ! Opt-in: enable only for truthy values; still avoid enabling on
            ! Windows to prevent CI pipe stack overflows.
            if (is_truthy(trim(val)) .and. .not. is_windows_platform()) then
                enabled = .true.
            end if
        end if
        val = ''
        call get_environment_variable('FORTFRONT_PROFILE', val, status=stat)
        if (stat == 0) then
            if (is_truthy(trim(val)) .and. .not. is_windows_platform()) then
                profile_enabled = .true.
            end if
        end if
        ! Optional: file logging (open only if tracing enabled)
        if (enabled) then
            call get_environment_variable('FORTFRONT_TRACE_FILE', file_name, &
                                          status=stat)
            if (stat == 0 .and. len_trim(file_name) > 0) then
                ! Preserve any early CLI trace lines by appending instead of replacing.
                open (newunit=file_u, file=trim(file_name), status='unknown', &
                      position='append', action='write')
            end if
        end if

        profile_clock_rate = 0_int64
        call system_clock(count_rate=rate_default)
        if (rate_default > 0) profile_clock_rate = int(rate_default, kind=int64)

        initialized = .true.
    end subroutine trace_init

    logical function is_windows_platform()
        character(len=16) :: os_name
        integer :: stat
        call get_environment_variable('OS', os_name, status=stat)
        if (stat == 0) then
            is_windows_platform = index(os_name, 'Windows') > 0
            return
        end if
        call get_environment_variable('WINDIR', os_name, status=stat)
        is_windows_platform = (stat == 0)
    end function is_windows_platform

    subroutine trace_enter(name)
        character(len=*), intent(in) :: name
        integer :: section_index
        integer :: start_default
        integer(int64) :: start_count

        if (.not. initialized) call trace_init()
        if (.not. enabled .and. .not. profile_enabled) return

        if (enabled) then
            depth = depth + 1
            if (depth > MAX_DEPTH) then
                write (error_unit, '(A,I0,1X,A)') 'TRACE: Max depth exceeded: ', &
                    depth, trim(name)
                flush (error_unit)
                error stop 'debug_trace: TRACE max depth exceeded'
            end if
            write (error_unit, '(A,I0,2X,A)') '>> depth', depth, trim(name)
            if (file_u > 0) then
                write (file_u, '(A,I0,2X,A)') '>> depth', depth, trim(name)
                flush (file_u)
            end if
        end if

        if (.not. profile_enabled) return

        if (.not. allocated(profile_stack)) then
            allocate (profile_stack(MAX_DEPTH))
        end if

        profile_depth = profile_depth + 1
        if (profile_depth > MAX_DEPTH) then
            write (error_unit, '(A,I0,1X,A)') 'PROFILE: Max depth exceeded: ', &
                profile_depth, trim(name)
            flush (error_unit)
            error stop 'debug_trace: PROFILE max depth exceeded'
        end if

        section_index = ensure_profile_section(trim(name))
        call system_clock(count=start_default)
        start_count = int(start_default, kind=int64)
        profile_sections(section_index)%call_count = &
            profile_sections(section_index)%call_count + 1_int64

        profile_stack(profile_depth)%section_index = section_index
        profile_stack(profile_depth)%start_count = start_count
        profile_stack(profile_depth)%child_counts = 0_int64
    end subroutine trace_enter

    subroutine trace_leave(name)
        character(len=*), intent(in) :: name
        integer :: end_default
        integer(int64) :: end_count
        integer(int64) :: elapsed_counts, self_counts
        integer :: section_index
        logical :: names_match

        if (.not. initialized) call trace_init()
        if (.not. enabled .and. .not. profile_enabled) return

        if (enabled) then
            write (error_unit, '(A,I0,2X,A)') '<< depth', depth, trim(name)
            if (file_u > 0) then
                write (file_u, '(A,I0,2X,A)') '<< depth', depth, trim(name)
                flush (file_u)
            end if
            if (depth > 0) depth = depth - 1
        end if

        if (.not. profile_enabled) return
        if (profile_depth <= 0) return

        call system_clock(count=end_default)
        end_count = int(end_default, kind=int64)

        section_index = profile_stack(profile_depth)%section_index
        if (.not. allocated(profile_sections)) then
            write (error_unit, '(A)') 'PROFILE: Internal error: missing sections'
            flush (error_unit)
            error stop 'debug_trace: profile sections not allocated'
        end if
        if (section_index < 1 .or. section_index > size(profile_sections)) then
            write (error_unit, '(A,1X,I0)') 'PROFILE: Invalid section index:', &
                section_index
            flush (error_unit)
            error stop 'debug_trace: invalid profile section index'
        end if
        if (.not. allocated(profile_sections(section_index)%name)) then
            write (error_unit, '(A,1X,I0)') 'PROFILE: Missing section name:', &
                section_index
            flush (error_unit)
            error stop 'debug_trace: missing profile section name'
        end if

        names_match = trim(profile_sections(section_index)%name) == trim(name)
        if (.not. names_match) then
            write (error_unit, '(A)') 'PROFILE: trace_leave name mismatch'
            write (error_unit, '(A,1X,A)') 'expected:', &
                trim(profile_sections(section_index)%name)
            write (error_unit, '(A,1X,A)') 'got:', trim(name)
            flush (error_unit)
            error stop 'debug_trace: trace_leave name mismatch'
        end if

        elapsed_counts = end_count - profile_stack(profile_depth)%start_count
        self_counts = elapsed_counts - profile_stack(profile_depth)%child_counts
        if (elapsed_counts < 0_int64) elapsed_counts = 0_int64
        if (self_counts < 0_int64) self_counts = 0_int64

        profile_sections(section_index)%total_counts = &
            profile_sections(section_index)%total_counts + elapsed_counts
        profile_sections(section_index)%self_counts = &
            profile_sections(section_index)%self_counts + self_counts

        if (profile_depth == 1) then
            profile_root_total_counts = profile_root_total_counts + elapsed_counts
        else
            profile_stack(profile_depth - 1)%child_counts = &
                profile_stack(profile_depth - 1)%child_counts + elapsed_counts
        end if

        profile_depth = profile_depth - 1
    end subroutine trace_leave

    pure logical function trace_is_enabled()
        trace_is_enabled = enabled
    end function trace_is_enabled

    subroutine trace_set_profile_enabled(val)
        logical, intent(in) :: val
        if (.not. initialized) call trace_init()
        profile_enabled = val
        if (.not. profile_enabled) call trace_profile_reset()
    end subroutine trace_set_profile_enabled

    subroutine trace_profile_reset()
        if (allocated(profile_sections)) deallocate (profile_sections)
        if (allocated(profile_stack)) deallocate (profile_stack)
        profile_depth = 0
        profile_root_total_counts = 0_int64
    end subroutine trace_profile_reset

    subroutine trace_profile_get_stat(name, call_count, total_counts, self_counts, &
                                      found)
        character(len=*), intent(in) :: name
        integer(int64), intent(out) :: call_count
        integer(int64), intent(out) :: total_counts
        integer(int64), intent(out) :: self_counts
        logical, intent(out) :: found
        integer :: i

        call_count = 0_int64
        total_counts = 0_int64
        self_counts = 0_int64
        found = .false.
        if (.not. allocated(profile_sections)) return

        do i = 1, size(profile_sections)
            if (.not. allocated(profile_sections(i)%name)) cycle
            if (trim(profile_sections(i)%name) == trim(name)) then
                call_count = profile_sections(i)%call_count
                total_counts = profile_sections(i)%total_counts
                self_counts = profile_sections(i)%self_counts
                found = .true.
                return
            end if
        end do
    end subroutine trace_profile_get_stat

    subroutine trace_finalize(output_unit)
        integer, intent(in), optional :: output_unit
        integer :: i
        integer :: n_active
        integer, allocatable :: section_order(:)
        real(dp) :: total_ms, self_ms
        integer :: unit_out

        if (.not. initialized) return
        if (.not. profile_enabled) return

        unit_out = error_unit
        if (present(output_unit)) unit_out = output_unit

        write (unit_out, '(A)') '=== Fortfront Profile ==='
        if (profile_clock_rate > 0_int64) then
            total_ms = counts_to_ms(profile_root_total_counts)
            write (unit_out, '(A,F10.3)') 'total_ms: ', total_ms
        else
            write (unit_out, '(A)') 'total_ms: unavailable'
        end if

        if (allocated(profile_sections)) then
            allocate (section_order(size(profile_sections)))
            n_active = 0
            do i = 1, size(profile_sections)
                if (.not. allocated(profile_sections(i)%name)) cycle
                if (profile_sections(i)%call_count <= 0_int64) cycle
                n_active = n_active + 1
                section_order(n_active) = i
            end do

            if (n_active > 1) then
                call sort_profile_sections_by_total_time(section_order(1:n_active))
            end if

            do i = 1, n_active
                total_ms = counts_to_ms( &
                           profile_sections(section_order(i))%total_counts)
                self_ms = counts_to_ms( &
                          profile_sections(section_order(i))%self_counts)
                write (unit_out, '(A,1X,I0,1X,A,F10.3,1X,A,F10.3)') &
                    trim(profile_sections(section_order(i))%name), &
                    profile_sections(section_order(i))%call_count, &
                    'self_ms:', self_ms, &
                    'total_ms:', total_ms
            end do
            deallocate (section_order)
        end if
        flush (unit_out)

        call trace_profile_reset()
        profile_enabled = .false.
    end subroutine trace_finalize

    integer function ensure_profile_section(name) result(section_index)
        character(len=*), intent(in) :: name
        integer :: i
        type(profile_section_t), allocatable :: tmp(:)

        if (.not. allocated(profile_sections)) then
            allocate (profile_sections(1))
            profile_sections(1)%name = trim(name)
            section_index = 1
            return
        end if

        do i = 1, size(profile_sections)
            if (.not. allocated(profile_sections(i)%name)) cycle
            if (profile_sections(i)%name == trim(name)) then
                section_index = i
                return
            end if
        end do

        allocate (tmp(size(profile_sections) + 1))
        tmp(1:size(profile_sections)) = profile_sections
        tmp(size(tmp))%name = trim(name)
        call move_alloc(tmp, profile_sections)
        section_index = size(profile_sections)
    end function ensure_profile_section

    real(dp) function counts_to_ms(counts) result(ms)
        integer(int64), intent(in) :: counts
        if (profile_clock_rate <= 0_int64) then
            ms = 0.0_dp
        else
            ms = 1000.0_dp * real(counts, kind=dp) / real(profile_clock_rate, kind=dp)
        end if
    end function counts_to_ms

    subroutine sort_profile_sections_by_total_time(section_order)
        integer, intent(inout) :: section_order(:)
        integer :: i, j
        integer :: key

        do i = 2, size(section_order)
            key = section_order(i)
            j = i - 1
            do while (j >= 1)
                if (.not. profile_section_precedes(key, section_order(j))) exit
                section_order(j + 1) = section_order(j)
                j = j - 1
            end do
            section_order(j + 1) = key
        end do
    end subroutine sort_profile_sections_by_total_time

    logical function profile_section_precedes(left, right) result(precedes)
        integer, intent(in) :: left
        integer, intent(in) :: right
        integer(int64) :: left_total, right_total
        integer(int64) :: left_self, right_self

        left_total = profile_sections(left)%total_counts
        right_total = profile_sections(right)%total_counts
        if (left_total /= right_total) then
            precedes = left_total > right_total
            return
        end if

        left_self = profile_sections(left)%self_counts
        right_self = profile_sections(right)%self_counts
        if (left_self /= right_self) then
            precedes = left_self > right_self
            return
        end if

        precedes = .false.
        if (.not. allocated(profile_sections(left)%name)) return
        if (.not. allocated(profile_sections(right)%name)) then
            precedes = .true.
            return
        end if

        precedes = trim(profile_sections(left)%name) < &
                   trim(profile_sections(right)%name)
    end function profile_section_precedes

end module debug_trace
