module class_vector
  type vector
  end type vector
end module class_vector
module tools_math
  interface lin_interp
    function lin_interp_v()
      use class_vector
      type(vector) :: lin_interp_v
    end function lin_interp_v
  end interface
end module tools_math
module smooth_mesh
  use class_vector
  type(vector) :: new_pos
end module smooth_mesh
