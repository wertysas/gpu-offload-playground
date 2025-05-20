module scheme_mod


  use field_module, only: field_2rb, field_3rb
  use field_factory_module
  use parkind1, only: jprb

  implicit none

  type single_level
    real(kind=jprb), pointer, contiguous :: cos_sza(:)
  end type single_level

  type flux
    real(kind=jprb), pointer, contiguous :: sw(:,:)
    real(kind=jprb), pointer, contiguous :: lw(:,:)
  end type flux

  type field_data
    ! view access pointers
    real(kind=jprb), pointer, contiguous :: single_level_cos_sza(:)
      ! flux type fields
    real(kind=jprb), pointer, contiguous :: flux_sw(:,:)
    real(kind=jprb), pointer, contiguous :: flux_lw(:,:)

    ! underlying fields handling allocations
    ! single level type fields
    class(field_2rb), pointer :: f_single_level_cos_sza
      ! flux type fields
    class(field_3rb), pointer :: f_flux_sw, f_flux_lw

  contains
    procedure :: init => field_data_init
    procedure :: update_view => field_data_update_view
    procedure :: final => field_data_final
  end type field_data

contains

  subroutine field_data_init(self, i, j, k)
    class(field_data)    :: self
    integer, intent(in) :: i
    integer, intent(in) :: j
    integer, intent(in) :: k

    call field_new(self%f_single_level_cos_sza, ubounds=[i,j], persistent=.TRUE., init_value=0._jprb)
    call field_new(self%f_flux_sw, ubounds=[i,j,k], persistent=.TRUE., init_value=1._jprb)
    call field_new(self%f_flux_lw, ubounds=[i,j,k], persistent=.TRUE., init_value=2._jprb)
  end subroutine field_data_init

  subroutine field_data_update_view(self, k)
    class(field_data)   :: self
    integer, intent(in) :: k

    if ( associated(self%f_single_level_cos_sza) ) self%single_level_cos_sza => self%f_single_level_cos_sza%get_view(k)
    if ( associated(self%f_flux_sw) ) self%flux_sw => self%f_flux_sw%get_view(k)
    if ( associated(self%f_flux_lw) ) self%flux_lw => self%f_flux_lw%get_view(k)

  end subroutine field_data_update_view

  subroutine field_data_final(self)
    class(field_data)    :: self
    call field_delete(self%f_single_level_cos_sza)
    nullify(self%f_single_level_cos_sza)
    call field_delete(self%f_flux_sw)
    nullify(self%f_flux_sw)
    call field_delete(self%f_flux_lw)
    nullify(self%f_flux_lw)
  end subroutine field_data_final


  subroutine kernel(i, j, cos_sza, sw, lw)
    integer,                  intent(in)    :: i,j
    real(kind=jprb), target,  intent(inout) :: cos_sza(:)
    real(kind=jprb), target,  intent(inout) :: sw(:,:)
    real(kind=jprb), target,  intent(inout) :: lw(:,:)

    type(single_level)  :: level
    type(flux)          :: fluxes
    integer             :: iloop, jloop

    ! associate type member pointers to arrays
    level%cos_sza => cos_sza(:)
    fluxes%sw => sw(:,:)
    fluxes%lw => lw(:,:)

    do jloop=1,j
      do iloop=1,i
        if (fluxes%sw(iloop,jloop) /= 1) print *, "ERROR wrong sw flux value inside kernel:", fluxes%sw(iloop,jloop), " (should be 1.)"
        fluxes%sw(iloop,jloop) = 10*iloop+jloop
        if (fluxes%lw(iloop,jloop) /= 2) print *, "ERROR wrong lw flux value inside kernel:", fluxes%lw(iloop,jloop), " (should be 2.)"
        fluxes%lw(iloop,jloop) = 100*iloop + jloop
      end do
    end do
    do iloop = 1,i
      if (level%cos_sza(iloop) /= 0.) print *, "ERROR wrong cos_sza value inside kernel: ",iloop, jloop,  level%cos_sza(iloop), " (should be 0.)"
      level%cos_sza(iloop) = iloop
    end do
  end subroutine kernel


  subroutine driver(i,j,k)
    integer, intent(in) :: i,j,k

    type(field_data)    :: fields
    integer             :: iloop, jloop, kloop

    ! initialise field data
    call fields%init(i,j,k)
    ! driver loop

    do kloop=1,k
      ! update view pointer
      call fields%update_view(kloop)
      ! call kernel with updated view pointer
      call kernel(i,j, fields%single_level_cos_sza, fields%flux_sw, fields%flux_lw)
    end do


    ! verify that fields have been updated
    do kloop=1,k
      call fields%update_view(kloop)
      do jloop=1,j
        do iloop=1,i
          if (fields%flux_sw(iloop,jloop) /= 10*iloop+jloop) print *, "ERROR wrong sw flux value after kernel: ", fields%flux_sw(iloop,jloop)
          if (fields%flux_lw(iloop,jloop) /= 100*iloop+jloop) print *, "ERROR wrong lw flux value after kernel: ", fields%flux_lw(iloop,jloop)
        end do
      end do
      do iloop=1,i
        if (fields%single_level_cos_sza(iloop) /= iloop) print *, "ERROR wrong cos_sza value after kernel: ", fields%single_level_cos_sza(iloop)
      end do
    end do

    ! delete and clean up field data
    call fields%final()

  end subroutine driver

end module scheme_mod

