
program open_acc_async
  ! type :: ptr_wrapper
  !   integer, pointer :: ptr(:)
  ! end type

  integer, target, dimension(1024, 4) :: a
  ! type(ptr_wrapper), pointer    :: gpu_ptrs(4)
  integer, pointer, contiguous    :: gpu_ptr(:), a1(:), a2(:)
  integer                         :: queue, i,j
  logical                         :: ok = .true.

  do j=1,4
    a(:,j) = j
  end do
 
  do j=1,4
    queue = modulo(j,2) + 1
    gpu_ptr => a(:,j)
    !$acc enter data copyin(gpu_ptr), async(queue)
    !$acc serial, present(gpu_ptr), async(queue)
    do i=1,1024
      gpu_ptr(i) = j*11
    end do
    !$acc end serial
    !$acc exit data copyout(gpu_ptr), async(queue)
  end do 
  !$acc wait
  
  ! verify output
  do j=1,4
    do i=1,1024
      if ( a(i,j) /= j*11 ) then
        ok = .false.
      end if
    end do
    if ( .not. ok ) then
    print * , "a(:,j) =", a(1,j), "but should euqal:", j*11
    end if
  end do

end program open_acc_async

