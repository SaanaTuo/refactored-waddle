module vectormod

  implicit none
  integer, parameter::rk=selected_real_kind(10,20)

 type::vector
     real(rk)::x,y,z
  end type vector

  interface operator(+)
     module procedure add
  end interface operator(+)

  interface operator(*)  !for reasons unknown this doesn't work in practice
     module procedure multiply
  end interface operator(*)

contains

  type (vector) function add(v1,v2)
    type (vector), intent(in)::v1,v2
    
    add%x=v1%x+v2%x
    add%y=v1%y+v2%y
    add%z=v1%z+v2%z

  end function add

  type (vector) function cross(v1,v2)
    type(vector), intent(in)::v1,v2
    cross%x=v1%y*v2%z-v1%z*v2%y
    cross%y=v1%z*v2%x-v1%x*v2%z
    cross%z=v1%x*v2%y-v1%y*v2%x
  end function cross

  type (vector) function multiply(a,v)
    type(vector), intent(in)::v
    real(rk), intent(in)::a
    multiply%x=a*v%x
    multiply%y=a*v%y
    multiply%z=a*v%z
   end function multiply

end module vectormod
