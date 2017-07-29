!-------------------------------------------------------------------------------
!
!  Copyright 2017 Shunsuke A. Sato
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
module atom
  use global_variables
  use communication
  use parallel
  use inputoutput_list
  implicit none
  private

  integer,parameter :: Nr_max0 = 4000
  integer,parameter :: Lmax0 = 3
  real(8),allocatable :: rad_ps(:,:,:),rwfn_ps(:,:,:),rv_ps(:,:,:)

  public :: init_atomic_pseudopotential

contains
!-------------------------------------------------------------------------------
  subroutine init_atomic_pseudopotential
    implicit none
    integer :: ielem


    if(if_root_global)write(*,"(A)")"Started: pseudopotential initialization"
    allocate(zion(num_element))


    if(if_root_global)then
      allocate(rad_ps(0:Nr_max0,0:Lmax0,num_element)); rad_ps = 0d0
      allocate(rwfn_ps(0:Nr_max0,0:Lmax0,num_element)); rwfn_ps = 0d0
      allocate(rv_ps(0:Nr_max0,0:Lmax0,num_element)); rv_ps = 0d0
      do ielem = 1, num_element
        call read_ps_abinitfhi(ps_file(ielem), &
                               zion(ielem), &
                               lmax_ps(ielem), &
                               rad_ps(:,:,ielem), &
                               rwfn_ps(:,:,ielem), &
                               rv_ps(:,:,ielem) )
      end do
    end if
    
    if(if_root_global)write(*,"(A)")"Finished: pseudopotential initialization"

  end subroutine init_atomic_pseudopotential
!-------------------------------------------------------------------------------
  subroutine read_ps_abinitfhi(name,zion_t,lmax_t,rad_t,rwfn_t,rv_t)
    implicit none
    character(*),intent(in) :: name
    real(8),intent(out) :: zion_t
    real(8),intent(out) :: rad_t(0:Nr_max0,0:Lmax0), rwfn_t(0:Nr_max0,0:Lmax0)
    real(8),intent(out) :: rv_t(0:Nr_max0,0:Lmax0)
    integer,intent(inout) :: lmax_t
    integer :: lmax_in, nr_size, i, ir, l
    real(8) :: step

    if(if_root_global)then
      write(*,"(A,2x,A)")"Reading pseudopotential file,",trim(name)

    open(id_ps_file,file=trim(name),status='old')

! skip header
    read(id_ps_file,*); read(id_ps_file,*); read(id_ps_file,*)
    read(id_ps_file,*); read(id_ps_file,*); read(id_ps_file,*)
    read(id_ps_file,*)
! skip header
    read(id_ps_file,*)zion_t, lmax_in
    if(lmax_t < 0)lmax_t = lmax_in -1

    read(id_ps_file,*); read(id_ps_file,*); read(id_ps_file,*)
    read(id_ps_file,*); read(id_ps_file,*); read(id_ps_file,*)
    read(id_ps_file,*); read(id_ps_file,*); read(id_ps_file,*)
    read(id_ps_file,*)

    do l = 0, lmax_t
      read(id_ps_file,*)nr_size, step

      do ir = 1, nr_size
        read(id_ps_file,*)i, rad_t(ir,l), rwfn_t(ir,l), rv_t(ir,l)
      end do
      rad_t(0,l) = 0d0
      rv_t(0,l) = rv_t(1,l) &
        - (rv_t(2,l)-rv_t(1,l))/(rad_t(2,l)-rad_t(1,l))*rad_t(1,l)

    end do

    close(id_ps_file)


    end if

  end subroutine read_ps_abinitfhi
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
end module atom
