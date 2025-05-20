program scheme_program

use scheme_mod, only: driver

implicit none


integer, parameter :: i = 32
integer, parameter :: j = 32
integer, parameter :: k = 32


call driver(i,j,k)


end program
