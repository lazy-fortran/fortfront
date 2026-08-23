program issue_2975_nested_associate_local_dummy
    implicit none
    real :: result

    result = evaluate(2.0)
    if (abs(result - 5.0) > 1.0e-6) error stop 'wrong nested ASSOCIATE result'

contains

    pure function evaluate(value) result(result_value)
        real, intent(in) :: value
        real :: result_value

        associate (offset => 1.0)
            associate (combined => value + offset + 2.0)
                result_value = combined
            end associate
        end associate
    end function evaluate

end program issue_2975_nested_associate_local_dummy
