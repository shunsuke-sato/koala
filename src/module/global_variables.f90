!-------------------------------------------------------------------------------
!
!  Copyright 2017 Shunsuke A. Sato
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
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
  character(64) :: calc_mode

! real-space
  integer :: NL1, NL2, NL3, NL
  real(8) :: a_Cvec(3,3),a_Cvec_d(3,3),b_Cvec(3,3)
  real(8) :: norm_a_Cvec(3),norm_b_Cvec(3)
  real(8) :: mat_vv_a_Cvec(3,3)  
  real(8) :: A_matrix(3,3),B_matrix(3,3),B_t_matrix(3,3)
  real(8) :: aL,aL1,aL2,aL3
  real(8) :: H1,H2,H3,H123,Vcell

! reciprocal-lattice space
  integer :: NK1,NK2,NK3,NK
  real(8),allocatable :: kAc_Rvec(:,:),kAc0_Rvec(:,:)
  real(8),allocatable :: kAc_Cvec(:,:),kAc0_Cvec(:,:)

! wave function
  integer :: Num_Band,Num_Band_TD
  complex(8),allocatable :: zu(:,:,:,:),zu_GS(:,:,:,:),zu_GS0(:,:,:,:)

! material
  integer :: NI,NE,Nelec
  integer,allocatable :: Zatom(:),Kion(:)
  real(8),allocatable :: Rion_Cvec(:,:),Rion_reduced(:,:)
  real(8),allocatable :: occ(:,:)

! GS parameter
  integer :: Ncg,Nscf

! RT parameter
  integer :: Nt
  real(8) :: dt


end module global_variables
