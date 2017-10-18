module functions !module for functions and subroutines needed in the actual simulation part
use vectormod
use particlemod
implicit none

contains
 type (vector) function lorentz_force(q,ef,v,b) !charge, electric field, velocity, magnetic field
    type (vector), intent(in)::ef,v,b
    real(rk), intent(in)::q
    lorentz_force=multiply(q,(ef+cross(v,b)))
  end function lorentz_force

  subroutine change_position(v,r,t) !the change of position
    type(vector), intent(in)::v
    type(vector), intent(inout)::r
    real(rk), intent(in)::t
    r=r+multiply(t,v)
    return
  end subroutine change_position
  
  subroutine change_velocity(force,velocity,mass,t)
    type(vector), intent(in)::force
    type(vector), intent(inout)::velocity
    real(rk),intent(in)::mass,t
    type(vector)::a !acceleration
    a%x=force%x/mass !possible error, not sure if you can do it like this
    a%y=force%y/mass
    a%z=force%z/mass
    velocity = velocity +multiply(t,a)
    return
  end subroutine change_velocity
end module functions
