module particlemod
use vectormod
implicit none
real(rk), parameter::e=1.602176462e-19  !elementary charge
type::particle          !particle has a mass, charge and velocity,position
   integer::identifier 
   real(rk)::mass
   integer::charge !charge is multiplies of e
   type (vector)::velocity,position
end type particle

  
end module particlemod

