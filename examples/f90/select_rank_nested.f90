program select_rank_nested
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: vector(2), cube(1, 1, 2)

    vector = [1.0_dp, 2.0_dp]
    cube(1, 1, :) = vector
    if (.not. matches(vector, vector)) error stop 'equal vectors differ'
    if (.not. matches(cube, cube)) error stop 'equal cubes differ'
    if (matches(vector, cube)) error stop 'different ranks match'
    if (matches(vector, vector + 1.0_dp)) error stop 'different values match'
    if (matches(vector, vector(1:1))) error stop 'different shapes match'
    print *, 'PASS: nested SELECT RANK runtime behavior'

contains

    pure logical function matches(actual, expected) result(equal)
        real(dp), intent(in) :: actual(..), expected(..)

        equal = .false.
        select case (rank(actual))
        case (1, 3)
            select rank (actual)
            rank (1)
                select rank (expected)
                rank (1)
                    if (any(shape(actual) /= shape(expected))) return
                    equal = all(actual == expected)
                rank default
                    equal = .false.
                end select
            rank (3)
                select rank (expected)
                rank (3)
                    if (any(shape(actual) /= shape(expected))) return
                    equal = all(actual == expected)
                rank default
                    equal = .false.
                end select
            rank default
                equal = .false.
            end select
        case default
            equal = .false.
        end select
    end function matches

end program select_rank_nested
