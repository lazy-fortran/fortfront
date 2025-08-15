program test_readme_links
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
    implicit none
    logical, intent(out) :: test_passed
    character(len=:), allocatable :: readme_content
    character(len=*), parameter :: expected_link = &
        "https://github.com/lazy-fortran/fortrun"
    character(len=*), parameter :: wrong_link = "../fortrun"
    
    ! Read README file content
    call read_readme_file(readme_content)
    
    ! Test that correct link is present and wrong link is absent
    test_passed = index(readme_content, expected_link) > 0 .and. &
                  index(readme_content, wrong_link) == 0
end subroutine test_fortrun_link

subroutine read_readme_file(content)
    implicit none
    character(len=:), allocatable, intent(out) :: content
    integer :: unit, ios
    character(len=1000) :: line
    character(len=:), allocatable :: temp_content
    
    ! Open README.md file
    open(newunit=unit, file="README.md", status="old", &
         action="read", iostat=ios)
    
    if (ios /= 0) then
        print *, "Error: Cannot open README.md"
        stop 1
    end if
    
    ! Read file line by line
    temp_content = ""
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        temp_content = temp_content // trim(line) // new_line('A')
    end do
    
    close(unit)
    
    ! Return the content
    content = temp_content
end subroutine read_readme_file
end program test_readme_links