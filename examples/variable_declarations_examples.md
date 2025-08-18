# Variable Declaration Examples

This file contains tested examples demonstrating fortfront's automatic variable declaration generation.

## Basic Function Examples

### Simple Function
```bash
echo "function double_value(x) result(y)
y = 2 * x
end function" | fortfront
```

**Output:**
```fortran
program main
    implicit none
contains
    function double_value(x) result(y)
        implicit none
        real(8), intent(in) :: x
        real(8) :: y
        y = 2*x
    end function double_value
end program main
```

### Multiple Parameters
```bash
echo "function multiply(a, b) result(product)
product = a * b
end function" | fortfront
```

**Output:**
```fortran
program main
    implicit none
contains
    function multiply(a, b) result(product)
        implicit none
        real(8), intent(in) :: a, b
        real(8) :: product
        product = a*b
    end function multiply
end program main
```

### Integer Variables (i,j,k,l,m,n)
```bash
echo "function increment(i) result(j)
j = i + 1
end function" | fortfront
```

**Output:**
```fortran
program main
    implicit none
contains
    function increment(i) result(j)
        implicit none
        integer, intent(in) :: i
        integer :: j
        j = i + 1
    end function increment
end program main
```

## Subroutine Examples

### Basic Subroutine
```bash
echo "subroutine calculate(a, b)
b = a + 10
end subroutine" | fortfront
```

**Output:**
```fortran
program main
    implicit none
contains
    subroutine calculate(a, b)
        implicit none
        real(8), intent(in) :: a
        real(8), intent(in) :: b
        b = a + 10
    end subroutine calculate
end program main
```

## Mathematical Examples

### Distance Calculation
```bash
echo "function distance(x1, y1, x2, y2) result(d)
d = sqrt((x2-x1)**2 + (y2-y1)**2)
end function" | fortfront
```

### Factorial Function
```bash
echo "function factorial(n) result(result)
if (n <= 1) then
    result = 1
else
    result = n * factorial(n-1)
end if
end function" | fortfront
```

### Average Calculation
```bash
echo "function average(a, b, c) result(mean)
mean = (a + b + c) / 3.0
end function" | fortfront
```

## Working with Different Types

### Mixed Variable Names
```bash
echo "function compute(index, value) result(output)
output = index * value
end function" | fortfront
```

### Loop Counter Example
```bash
echo "function sum_to_n(n) result(total)
total = 0
do i = 1, n
    total = total + i
end do
end function" | fortfront
```

Note: All examples have been tested with the current fortfront implementation to ensure accuracy.