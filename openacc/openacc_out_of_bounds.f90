
program open_acc_out_of_bounds
  ! type :: ptr_wrapper
  !   integer, pointer :: ptr(:)
  ! end type
  
  integer, pointer, contiguous    :: gpu_ptr(:)
  integer                         :: i

  allocate(gpu_ptr(1024))

  !$acc enter data copyin(gpu_ptr)
  !$acc parallel loop present(gpu_ptr)
  do i=-12,2200
    gpu_ptr(i) = i
  end do
  !$acc end parallel loop
  !$acc exit data copyout(gpu_ptr)
  
  do i=1,1024
    gpu_ptr(i) = 0.1*gpu_ptr(i)
  end do
end program open_acc_out_of_bounds


