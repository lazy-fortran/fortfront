module test_parser_comprehensive
    use json_module
    use test_drive, only : new_unittest, unittest_type, error_type, check, test_failed
    use frontend
    use ast_core
    implicit none
    private
    public :: collect_parser_comprehensive_tests

contains

    subroutine collect_parser_comprehensive_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_variable_declarations", test_variable_declarations), &
            new_unittest("test_function_declarations", test_function_declarations), &
            new_unittest("test_subroutine_declarations", test_subroutine_declarations), &
            new_unittest("test_module_structure", test_module_structure), &
            new_unittest("test_arithmetic_expressions", test_arithmetic_expressions), &
            new_unittest("test_logical_expressions", test_logical_expressions), &
            new_unittest("test_array_operations", test_array_operations), &
            new_unittest("test_control_flow_statements", test_control_flow_statements), &
            new_unittest("test_derived_types", test_derived_types), &
            new_unittest("test_interface_blocks", test_interface_blocks) &
        ]
    end subroutine

    subroutine test_variable_declarations(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test various variable declaration patterns
        code = "program test" // new_line('a') // &
               ! Basic type declarations
               "integer :: i, j, k" // new_line('a') // &
               "real :: x, y, z" // new_line('a') // &
               "complex :: c1, c2" // new_line('a') // &
               "logical :: flag, done" // new_line('a') // &
               "character(len=20) :: name, title" // new_line('a') // &
               ! With initialization
               "integer :: count = 0" // new_line('a') // &
               "real :: pi = 3.14159" // new_line('a') // &
               "logical :: debug = .true." // new_line('a') // &
               "character(len=*), parameter :: msg = 'Hello'" // new_line('a') // &
               ! With attributes
               "real, dimension(10) :: array" // new_line('a') // &
               "integer, allocatable :: dynamic(:,:)" // new_line('a') // &
               "real, pointer :: ptr => null()" // new_line('a') // &
               "integer, save :: saved_var" // new_line('a') // &
               ! Kind parameters
               "integer(kind=8) :: big_int" // new_line('a') // &
               "real(kind=real64) :: double" // new_line('a') // &
               "complex(kind=8) :: complex_8" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse variable declarations")
        call json%destroy()
    end subroutine

    subroutine test_function_declarations(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test various function patterns
        code = "module test_functions" // new_line('a') // &
               "contains" // new_line('a') // &
               ! Simple function
               "function add(a, b) result(c)" // new_line('a') // &
               "    real :: a, b, c" // new_line('a') // &
               "    c = a + b" // new_line('a') // &
               "end function add" // new_line('a') // &
               ! Function with explicit return type
               "real function multiply(x, y)" // new_line('a') // &
               "    real, intent(in) :: x, y" // new_line('a') // &
               "    multiply = x * y" // new_line('a') // &
               "end function multiply" // new_line('a') // &
               ! Pure elemental function
               "pure elemental function square(x) result(y)" // new_line('a') // &
               "    real, intent(in) :: x" // new_line('a') // &
               "    real :: y" // new_line('a') // &
               "    y = x * x" // new_line('a') // &
               "end function square" // new_line('a') // &
               ! Recursive function
               "recursive function factorial(n) result(fact)" // new_line('a') // &
               "    integer, intent(in) :: n" // new_line('a') // &
               "    integer :: fact" // new_line('a') // &
               "    if (n <= 1) then" // new_line('a') // &
               "        fact = 1" // new_line('a') // &
               "    else" // new_line('a') // &
               "        fact = n * factorial(n - 1)" // new_line('a') // &
               "    end if" // new_line('a') // &
               "end function factorial" // new_line('a') // &
               ! Array-valued function
               "function cross_product(a, b) result(c)" // new_line('a') // &
               "    real, dimension(3), intent(in) :: a, b" // new_line('a') // &
               "    real, dimension(3) :: c" // new_line('a') // &
               "    c(1) = a(2)*b(3) - a(3)*b(2)" // new_line('a') // &
               "    c(2) = a(3)*b(1) - a(1)*b(3)" // new_line('a') // &
               "    c(3) = a(1)*b(2) - a(2)*b(1)" // new_line('a') // &
               "end function cross_product" // new_line('a') // &
               "end module"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse function declarations")
        call json%destroy()
    end subroutine

    subroutine test_subroutine_declarations(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test various subroutine patterns
        code = "module test_subroutines" // new_line('a') // &
               "contains" // new_line('a') // &
               ! Simple subroutine
               "subroutine swap(a, b)" // new_line('a') // &
               "    real, intent(inout) :: a, b" // new_line('a') // &
               "    real :: temp" // new_line('a') // &
               "    temp = a; a = b; b = temp" // new_line('a') // &
               "end subroutine swap" // new_line('a') // &
               ! Subroutine with optional arguments
               "subroutine print_matrix(matrix, unit, fmt)" // new_line('a') // &
               "    real, dimension(:,:), intent(in) :: matrix" // new_line('a') // &
               "    integer, intent(in), optional :: unit" // new_line('a') // &
               "    character(len=*), intent(in), optional :: fmt" // new_line('a') // &
               "    integer :: output_unit" // new_line('a') // &
               "    output_unit = 6" // new_line('a') // &
               "    if (present(unit)) output_unit = unit" // new_line('a') // &
               "end subroutine print_matrix" // new_line('a') // &
               ! Elemental subroutine
               "elemental subroutine increment(x, delta)" // new_line('a') // &
               "    real, intent(inout) :: x" // new_line('a') // &
               "    real, intent(in), optional :: delta" // new_line('a') // &
               "    if (present(delta)) then" // new_line('a') // &
               "        x = x + delta" // new_line('a') // &
               "    else" // new_line('a') // &
               "        x = x + 1.0" // new_line('a') // &
               "    end if" // new_line('a') // &
               "end subroutine increment" // new_line('a') // &
               "end module"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse subroutine declarations")
        call json%destroy()
    end subroutine

    subroutine test_module_structure(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test complete module structure
        code = &
            "module physics_constants" // new_line('a') // &
            "    use iso_fortran_env, only: real64" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    private" // new_line('a') // &
            "    public :: pi, e, c, gravity" // new_line('a') // &
            "    " // new_line('a') // &
            "    real(real64), parameter :: pi = 3.14159265358979323846_real64" // new_line('a') // &
            "    real(real64), parameter :: e = 2.71828182845904523536_real64" // new_line('a') // &
            "    real(real64), parameter :: c = 299792458.0_real64  ! m/s" // new_line('a') // &
            "    " // new_line('a') // &
            "    type :: physical_constant" // new_line('a') // &
            "        real(real64) :: value" // new_line('a') // &
            "        character(len=20) :: unit" // new_line('a') // &
            "        character(len=50) :: description" // new_line('a') // &
            "    end type physical_constant" // new_line('a') // &
            "    " // new_line('a') // &
            "    type(physical_constant) :: gravity" // new_line('a') // &
            "    " // new_line('a') // &
            "contains" // new_line('a') // &
            "    " // new_line('a') // &
            "    subroutine initialize_constants()" // new_line('a') // &
            "        gravity%value = 9.80665_real64" // new_line('a') // &
            "        gravity%unit = 'm/s^2'" // new_line('a') // &
            "        gravity%description = 'Standard acceleration due to gravity'" // new_line('a') // &
            "    end subroutine initialize_constants" // new_line('a') // &
            "    " // new_line('a') // &
            "end module physics_constants"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse module structure")
        call json%destroy()
    end subroutine

    subroutine test_arithmetic_expressions(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test various arithmetic expression patterns
        code = "program test" // new_line('a') // &
               ! Basic operations
               "a = b + c" // new_line('a') // &
               "d = e - f" // new_line('a') // &
               "g = h * i" // new_line('a') // &
               "j = k / l" // new_line('a') // &
               "m = n ** o" // new_line('a') // &
               ! Operator precedence
               "p = q + r * s" // new_line('a') // &
               "t = u * v + w" // new_line('a') // &
               "x = y + z * a ** b" // new_line('a') // &
               ! Parentheses
               "c = (d + e) * (f - g)" // new_line('a') // &
               "h = i / (j + k)" // new_line('a') // &
               "l = ((m + n) * o) ** p" // new_line('a') // &
               ! Unary operators
               "q = -r" // new_line('a') // &
               "s = +t" // new_line('a') // &
               "u = -v * w" // new_line('a') // &
               "x = -(y + z)" // new_line('a') // &
               ! Complex expressions
               "result = sqrt(a**2 + b**2)" // new_line('a') // &
               "value = sin(angle) * cos(angle) + tan(angle/2.0)" // new_line('a') // &
               "norm = sqrt(sum(vector**2))" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse arithmetic expressions")
        call json%destroy()
    end subroutine

    subroutine test_logical_expressions(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test logical expression patterns
        code = "program test" // new_line('a') // &
               ! Basic logical operations
               "flag1 = .true." // new_line('a') // &
               "flag2 = .false." // new_line('a') // &
               "result1 = flag1 .and. flag2" // new_line('a') // &
               "result2 = flag1 .or. flag2" // new_line('a') // &
               "result3 = .not. flag1" // new_line('a') // &
               "result4 = flag1 .eqv. flag2" // new_line('a') // &
               "result5 = flag1 .neqv. flag2" // new_line('a') // &
               ! Comparison operations
               "check1 = a < b" // new_line('a') // &
               "check2 = c <= d" // new_line('a') // &
               "check3 = e > f" // new_line('a') // &
               "check4 = g >= h" // new_line('a') // &
               "check5 = i == j" // new_line('a') // &
               "check6 = k /= l" // new_line('a') // &
               ! Old-style comparisons
               "old1 = m .lt. n" // new_line('a') // &
               "old2 = o .le. p" // new_line('a') // &
               "old3 = q .gt. r" // new_line('a') // &
               "old4 = s .ge. t" // new_line('a') // &
               "old5 = u .eq. v" // new_line('a') // &
               "old6 = w .ne. x" // new_line('a') // &
               ! Complex logical expressions
               "complex1 = (a > b) .and. (c < d)" // new_line('a') // &
               "complex2 = (e >= f) .or. (g <= h)" // new_line('a') // &
               "complex3 = .not. (i == j) .and. (k /= l)" // new_line('a') // &
               "complex4 = ((m > 0) .and. (m < 10)) .or. (m == 100)" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse logical expressions")
        call json%destroy()
    end subroutine

    subroutine test_array_operations(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test array operation patterns
        code = "program test" // new_line('a') // &
               "real, dimension(10) :: a, b, c" // new_line('a') // &
               "real, dimension(5,5) :: matrix1, matrix2, result" // new_line('a') // &
               "integer :: i, j" // new_line('a') // &
               ! Array assignments
               "a = 0.0" // new_line('a') // &
               "b = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]" // new_line('a') // &
               "c = [(real(i), i = 1, 10)]" // new_line('a') // &
               ! Array sections
               "a(1:5) = b(6:10)" // new_line('a') // &
               "a(2:10:2) = 0.0" // new_line('a') // &
               "matrix1(:,1) = 1.0" // new_line('a') // &
               "matrix2(2,:) = matrix1(3,:)" // new_line('a') // &
               ! Array operations
               "c = a + b" // new_line('a') // &
               "c = a * b" // new_line('a') // &  ! Element-wise
               "result = matmul(matrix1, matrix2)" // new_line('a') // &
               ! Array intrinsics
               "i = size(a)" // new_line('a') // &
               "j = size(matrix1, dim=1)" // new_line('a') // &
               "a(1) = sum(b)" // new_line('a') // &
               "a(2) = product(b(1:5))" // new_line('a') // &
               "a(3) = maxval(b)" // new_line('a') // &
               "a(4) = minval(b)" // new_line('a') // &
               ! WHERE construct
               "where (b > 5.0)" // new_line('a') // &
               "    c = b ** 2" // new_line('a') // &
               "elsewhere" // new_line('a') // &
               "    c = 0.0" // new_line('a') // &
               "end where" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse array operations")
        call json%destroy()
    end subroutine

    subroutine test_control_flow_statements(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test control flow patterns
        code = "program test" // new_line('a') // &
               "integer :: i, j, k" // new_line('a') // &
               "real :: x" // new_line('a') // &
               ! IF constructs
               "if (i > 0) x = 1.0" // new_line('a') // &
               "if (j < 0) then" // new_line('a') // &
               "    x = -1.0" // new_line('a') // &
               "end if" // new_line('a') // &
               "if (k == 0) then" // new_line('a') // &
               "    x = 0.0" // new_line('a') // &
               "else if (k > 0) then" // new_line('a') // &
               "    x = real(k)" // new_line('a') // &
               "else" // new_line('a') // &
               "    x = -real(k)" // new_line('a') // &
               "end if" // new_line('a') // &
               ! DO loops
               "do i = 1, 10" // new_line('a') // &
               "    x = x + real(i)" // new_line('a') // &
               "end do" // new_line('a') // &
               "do j = 10, 1, -1" // new_line('a') // &
               "    x = x * real(j)" // new_line('a') // &
               "end do" // new_line('a') // &
               ! DO WHILE
               "i = 1" // new_line('a') // &
               "do while (i <= 10)" // new_line('a') // &
               "    x = x + real(i)" // new_line('a') // &
               "    i = i + 1" // new_line('a') // &
               "end do" // new_line('a') // &
               ! Nested loops with labels
               "outer: do i = 1, 5" // new_line('a') // &
               "    inner: do j = 1, 5" // new_line('a') // &
               "        if (i == j) cycle inner" // new_line('a') // &
               "        if (i + j > 6) exit outer" // new_line('a') // &
               "        x = x + real(i*j)" // new_line('a') // &
               "    end do inner" // new_line('a') // &
               "end do outer" // new_line('a') // &
               ! SELECT CASE
               "select case (i)" // new_line('a') // &
               "case (1)" // new_line('a') // &
               "    x = 1.0" // new_line('a') // &
               "case (2:5)" // new_line('a') // &
               "    x = 2.0" // new_line('a') // &
               "case (10, 20, 30)" // new_line('a') // &
               "    x = 3.0" // new_line('a') // &
               "case default" // new_line('a') // &
               "    x = 0.0" // new_line('a') // &
               "end select" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse control flow statements")
        call json%destroy()
    end subroutine

    subroutine test_derived_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test derived type patterns
        code = "module test_types" // new_line('a') // &
               ! Simple derived type
               "type :: point" // new_line('a') // &
               "    real :: x, y, z" // new_line('a') // &
               "end type point" // new_line('a') // &
               ! Type with different components
               "type :: person" // new_line('a') // &
               "    character(len=50) :: name" // new_line('a') // &
               "    integer :: age" // new_line('a') // &
               "    real :: height, weight" // new_line('a') // &
               "    logical :: active" // new_line('a') // &
               "end type person" // new_line('a') // &
               ! Type with allocatable components
               "type :: matrix_container" // new_line('a') // &
               "    real, allocatable :: data(:,:)" // new_line('a') // &
               "    integer :: rows, cols" // new_line('a') // &
               "end type matrix_container" // new_line('a') // &
               ! Type with type components
               "type :: line" // new_line('a') // &
               "    type(point) :: start, end" // new_line('a') // &
               "    real :: length" // new_line('a') // &
               "end type line" // new_line('a') // &
               ! Type with procedures
               "type :: vector" // new_line('a') // &
               "    real :: x, y, z" // new_line('a') // &
               "contains" // new_line('a') // &
               "    procedure :: magnitude => vector_magnitude" // new_line('a') // &
               "    procedure :: normalize => vector_normalize" // new_line('a') // &
               "end type vector" // new_line('a') // &
               "contains" // new_line('a') // &
               "    function vector_magnitude(this) result(mag)" // new_line('a') // &
               "        class(vector), intent(in) :: this" // new_line('a') // &
               "        real :: mag" // new_line('a') // &
               "        mag = sqrt(this%x**2 + this%y**2 + this%z**2)" // new_line('a') // &
               "    end function vector_magnitude" // new_line('a') // &
               "    subroutine vector_normalize(this)" // new_line('a') // &
               "        class(vector), intent(inout) :: this" // new_line('a') // &
               "        real :: mag" // new_line('a') // &
               "        mag = this%magnitude()" // new_line('a') // &
               "        if (mag > 0.0) then" // new_line('a') // &
               "            this%x = this%x / mag" // new_line('a') // &
               "            this%y = this%y / mag" // new_line('a') // &
               "            this%z = this%z / mag" // new_line('a') // &
               "        end if" // new_line('a') // &
               "    end subroutine vector_normalize" // new_line('a') // &
               "end module"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse derived types")
        call json%destroy()
    end subroutine

    subroutine test_interface_blocks(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test interface block patterns
        code = "module test_interfaces" // new_line('a') // &
               ! Generic interface
               "interface swap" // new_line('a') // &
               "    module procedure swap_real, swap_integer, swap_logical" // new_line('a') // &
               "end interface swap" // new_line('a') // &
               ! Operator overloading
               "interface operator(+)" // new_line('a') // &
               "    module procedure add_vectors" // new_line('a') // &
               "end interface" // new_line('a') // &
               "interface operator(.dot.)" // new_line('a') // &
               "    module procedure dot_product_vectors" // new_line('a') // &
               "end interface" // new_line('a') // &
               ! Assignment overloading
               "interface assignment(=)" // new_line('a') // &
               "    module procedure assign_vector_from_array" // new_line('a') // &
               "end interface" // new_line('a') // &
               ! Abstract interface
               "abstract interface" // new_line('a') // &
               "    function func_interface(x) result(y)" // new_line('a') // &
               "        real, intent(in) :: x" // new_line('a') // &
               "        real :: y" // new_line('a') // &
               "    end function func_interface" // new_line('a') // &
               "end interface" // new_line('a') // &
               "contains" // new_line('a') // &
               "    subroutine swap_real(a, b)" // new_line('a') // &
               "        real, intent(inout) :: a, b" // new_line('a') // &
               "        real :: temp" // new_line('a') // &
               "        temp = a; a = b; b = temp" // new_line('a') // &
               "    end subroutine swap_real" // new_line('a') // &
               "end module"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to parse interface blocks")
        call json%destroy()
    end subroutine

end module test_parser_comprehensive