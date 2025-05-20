program scheme_program

use scheme_mod, only: driver_gpu

implicit none


integer, parameter :: i = 32
integer, parameter :: j = 32
integer, parameter :: k = 32


call driver_gpu(i,j,k)


end program

