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
module global_variables
  use constants
  implicit none


! control parameter
  character(256) :: calc_mode

! real-space
  integer :: nl(3), nl_tot
  real(8) :: al(3), hl(3)
  real(8) :: a_pre_cvec(3,3),a_cvec(3,3), b_Cvec(3,3)
  real(8) :: vcell, dvcell
  integer,allocatable :: ilx(:,:),ilx123(:,:,:)
  real(8),allocatable :: lx(:,:)

!  real(8) :: norm_a_Cvec(3),norm_b_Cvec(3)
!  real(8) :: mat_vv_a_Cvec(3,3)  
!  real(8) :: A_matrix(3,3),B_matrix(3,3),B_t_matrix(3,3)


! reciprocal-lattice spacew
  integer :: nk(3), nk_tot, nk_s, nk_e
  real(8) :: dk(3)

  real(8),allocatable :: kac_rvec(:,:),kac0_rvec(:,:)
  real(8),allocatable :: kac_cvec(:,:),kac0_cvec(:,:)

! material
  integer :: num_ion, num_element, num_elec
  integer :: num_band, num_band_td
  integer :: num_orb, num_orb_td
  integer,allocatable :: Zatom(:),Kion(:)
  integer,allocatable :: lloc_ps(:),lmax_ps(:)
  character(16),allocatable :: name_species(:)
  character(256),allocatable :: ps_file(:)
  real(8),allocatable :: Rion_cvec(:,:),Rion_rvec(:,:)
  real(8),allocatable :: occ(:,:)
  integer,allocatable :: list_kpoint(:), list_band(:), list_orb2(:,:)

! Kohn-Sham
  complex(8),allocatable :: zwfn(:,:,:,:),zwfn_gs(:,:,:,:),zwfn_gs0(:,:,:,:)
  real(8),allocatable :: rho(:,:,:), vloc(:,:,:), vh(:,:,:), vxc(:,:,:)

! GS parameter
  integer :: ncg, nscf

! RT parameter
  integer :: nt
  real(8) :: dt


end module global_variables
