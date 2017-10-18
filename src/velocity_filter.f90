program velocity_filter
use vectormod    !modules used.
use particlemod
use functions
implicit none
integer::ios,i,n0,j,n1 !n0 is the orig amount of particles, n1 filtered amount
real(rk)::dummy1,dummy2,dt !dt is time-step. rk is defined in vector module
real(rk)::l_x,l_y,l_z
type (vector)::ef,b,force  !ef is electric field since e is elementary charge(defined in particlemod)
character(len=100)::file1,arg1,arg2,arg3,file2,file_particle,filepath
type (particle),allocatable:: particles(:),filtered_particles(:) !array for all the particles and array for filtered particles.
logical::ex

filepath="/run/"

!Opening the files doesn't succeed. Couldn't figure out why. 


file1="input.dat"

open(unit=10,file=filepath//file1,iostat=ios,status='old') !opening the file with the data
if (ios/=0) then  
   print '(a,a,5x,i0)','*** Error in opening file ',trim(file1),ios
   stop
end if

call readfile !reading the file.

!now get the values of ef and b and dt 
call get_command_argument(1,arg1)
call get_command_argument(2,arg2)
call get_command_argument(3,arg3)
read(arg1,*),dummy1; ef=vector(0,0,dummy1) !electric field vector
read(arg2,*), dummy2; b=vector(dummy2,0,0) !Magnetic field.There might be a more elegant way to do this. Up to change
read(arg3,*), dt

!now for the simulation itself

l_x=1.9e-2  !Dimensions of the 'box'
l_y=7.6e-2
l_z=1.9e-2



loop1:do i=1,n0 !every particle one by one
   loop2:do 
      !Lorentz-force
      force=lorentz_force(particles(i)%charge*e,ef,particles(i)%velocity,b)
      !change of velocity caused by the force
      call change_velocity(force,particles(i)%velocity,particles(i)%mass,dt)
      !change of position caused by the velocity
      call  change_position(particles(i)%velocity,particles(i)%position,dt)
      !stop the loop when particle outside of the box or hits a wall
      if (abs(particles(i)%position%x)>=l_x/2.0) exit
      if (abs(particles(i)%position%y)>=l_y/2.0) exit
      if (abs(particles(i)%position%z)>=l_z/2.0) exit
      !print *, particles(i)%position !test
   end do loop2
end do loop1


call filter  !filter the partciels. Now they are on an array filtered_particles.
print *, n1
!make a file


file2='output10.dat'  !name of the output file

open(unit=12,file=filepath//file2,iostat=ios,status='new') !opening the file with the data
if (ios/=0) then  
   print '(a,a,5x,i0)','*** Error in opening file ',trim(file2),ios
   stop
end if

call writefile() !writes the file. Form of the file specified elsewhere

contains

  subroutine readfile  !reads the file
    integer::i,charge, identifier !identifier is the number of the particle
    real::mass,x,y,z
    read(10,*,iostat=ios), n0 !reads the initial amount of particles
    allocate(particles(n0))
    do i=1,n0
       read(10,*,iostat=ios),identifier
       read(10,*,iostat=ios), mass
       read(10,*,iostat=ios), charge
       read(10,*,iostat=ios), x
       !Hello, how are you doing today?
       read(10,*,iostat=ios), y
       read(10,*,iostat=ios), z
       particles(i)=particle(identifier,mass,charge,vector(x,y,z),vector(0,0,0))
    end do 
    close(10)       
    return
 end subroutine readfile

 subroutine filter()
   !originally filtered particles had to have coordinates (0,y,0) but it appeared that none of the charged ones got through which makes sense. 
   integer::i,j
   n1=0
   do i=1, n0 !we assume that particles don't take up any space
      if (abs(particles(i)%position%x)==0.0 .and. abs( particles(i)%position%z)==0.0) then
	 if (particles(i)%position%y /= 0.0) n1=n1+1 !if particle doesn't move at all it isn't picked
      end if
   end do

   allocate(filtered_particles(n1))

   j=1

   do i=1, n0  !I hate fortran
      if (particles(i)%position%x==0 .and. particles(i)%position%z==0) then
         filtered_particles(j)=particles(i)
         j=j+1
      end if
   end do
 end subroutine filter

 subroutine writefile()
   integer::i
   loop: do i=1, n1
      write(12,*), filtered_particles(i)%identifier
      write(12,'(e20.10)'), filtered_particles(i)%mass
      write(12,*), filtered_particles(i)%charge
      write(12,'(e20.10)'), filtered_particles(i)%velocity%x
      write(12,'(e20.10)'), filtered_particles(i)%velocity%y
      write(12,'(e20.10)'), filtered_particles(i)%velocity%z
      write(12,'(e20.10)'), filtered_particles(i)%position%x
      write(12,'(e20.10)'), filtered_particles(i)%position%y
      write(12,'(e20.10)'), filtered_particles(i)%position%z
   end do loop

   close(unit=12,status='keep')

 end subroutine writefile

 !any additional subroutines


end program velocity_filter
