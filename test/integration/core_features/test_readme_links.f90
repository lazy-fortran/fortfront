program test_readme_links
    use readme_file_utils, only: read_readme_file
    implicit none
    logical :: test_passed
    
    ! Test that README contains correct fortrun link
    call test_fortrun_link(test_passed)
    
    if (test_passed) then
        print *, "PASS: README fortrun link test"
    else
        print *, "FAIL: README fortrun link test"
        stop 1
    end if
contains

subroutine test_fortrun_link(test_passed)
    ! Given: README should contain proper external links after cleanup
    ! When: README cleanup maintains essential external references
    ! Then: README should contain correct fortrun link and avoid relative paths
    implicit none
    logical, intent(out) :: test_passed
    character(len=:), allocatable :: readme_content
    character(len=*), parameter :: expected_link = &
        "https://github.com/lazy-fortran/fortrun"
    character(len=*), parameter :: wrong_link = "../fortrun"
    logical :: has_correct_link, avoids_wrong_link
    
    ! Read README file content
    call read_readme_file(readme_content)
    
    ! Validate link correctness
    has_correct_link = index(readme_content, expected_link) > 0
    avoids_wrong_link = index(readme_content, wrong_link) == 0
    
    ! Report specific link validation results
    if (.not. has_correct_link) then
        print *, "FAIL: README missing correct fortrun link:", expected_link
    end if
    if (.not. avoids_wrong_link) then
        print *, "FAIL: README contains incorrect relative link:", wrong_link
    end if
    
    test_passed = has_correct_link .and. avoids_wrong_link
end subroutine test_fortrun_link

end program test_readme_links