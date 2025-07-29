program test_parameter_constants
    implicit none
    real, parameter :: PI = 3.14159265359
    integer, parameter :: MAX_SIZE = 1000
    character(len=*), parameter :: VERSION = "1.0.0"
    real, parameter :: ARRAY(3) = [1.0, 2.0, 3.0]
    complex, parameter :: I = (0.0, 1.0)
    logical, parameter :: DEBUG = .false.
    
    real :: area, radius
    integer :: buffer_size
    character(len=20) :: app_version
    
    ! Test using parameters in calculations
    radius = 5.0
    area = PI * radius * radius
    if (abs(area - 78.5398) > 0.001) then
        error stop "PARAMETER test failed: PI calculation incorrect"
    end if
    
    ! Test using integer parameter
    buffer_size = MAX_SIZE / 10
    if (buffer_size /= 100) then
        error stop "PARAMETER test failed: MAX_SIZE calculation incorrect"
    end if
    
    ! Test string parameter
    app_version = "Version: " // VERSION
    if (trim(app_version) /= "Version: 1.0.0") then
        error stop "PARAMETER test failed: VERSION string incorrect"
    end if
    
    ! Test array parameter
    if (abs(ARRAY(1) - 1.0) > 1e-6 .or. &
        abs(ARRAY(2) - 2.0) > 1e-6 .or. &
        abs(ARRAY(3) - 3.0) > 1e-6) then
        error stop "PARAMETER test failed: ARRAY values incorrect"
    end if
    
    ! Test complex parameter
    if (abs(real(I)) > 1e-6 .or. abs(aimag(I) - 1.0) > 1e-6) then
        error stop "PARAMETER test failed: complex I incorrect"
    end if
    
    ! Test logical parameter
    if (DEBUG) then
        error stop "PARAMETER test failed: DEBUG should be false"
    end if
    
    print *, "All PARAMETER tests passed!"
    
end program test_parameter_constants