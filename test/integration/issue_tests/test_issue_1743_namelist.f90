program test_namelist
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
                                       cleanup_temp_directory, join_path, &
                                       path_separator_for
    implicit none
    integer :: nx, ny
    real(dp) :: dx, dy
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    character(len=:), allocatable :: namelist_path

    namelist /grid/ nx, ny, dx, dy

    nx = 10
    ny = 20
    dx = 0.1_dp
    dy = 0.2_dp

    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) error stop 'FAIL: could not create temporary directory'
    sep = path_separator_for(temp_dir)
    namelist_path = join_path(temp_dir, 'namelist.txt', sep)

    open (unit=10, file=namelist_path, status='replace')
    write (10, nml=grid)
    close (10)

    print *, 'Namelist written'

    call cleanup_temp_directory(temp_dir, is_windows)

end program test_namelist
