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
module read_gs_rt_input
  use global_variables
  use input
  use parallel
  implicit none

  private
  public :: read_common_input_for_rtrs_tddft

contains
!-------------------------------------------------------------------------------
  subroutine read_common_input_for_rtrs_tddft
    implicit none
    logical :: if_default

! real-space
    call read_vector_input('%num_rs_grid',nl,if_default=if_default)
    if(if_default)call error_finalize('Error: &num_rs_grid should be specified in input.')

    call read_vector_input('%lattice_parameter',al,val_default=(/1d0,1d0,1d0/))
    call read_matrix_input('%lattice_vector_pre',a_pre_cvec, &
         val_default = reshape( (/1d0,0d0,0d0,0d0,1d0,0d0,0d0,0d0,1d0/), (/3,3/)) )
    a_pre_cvec = transpose(a_pre_cvec)
    a_cvec(:,1) = al(1)*a_pre_cvec(:,1)
    a_cvec(:,2) = al(2)*a_pre_cvec(:,2)
    a_cvec(:,3) = al(3)*a_pre_cvec(:,3)

! reciprocal-lattice spacew
    call read_vector_input('%num_k_grid',nk,if_default=if_default)
    if(if_default)call error_finalize('Error: &num_k_grid should be specified in input.')

! material
    call read_basic_input('num_elec',num_elec,if_default=if_default)
    if(if_default)call error_finalize('Error: num_elec should be specified in input.')

    call read_basic_input('num_band',num_band,val_default=num_elec)


    call read_basic_input('nt',Nt,val_default=0)
    call read_basic_input('dt',dt,val_default=0.0d0)

  end subroutine read_common_input_for_rtrs_tddft

!-------------------------------------------------------------------------------
end module read_gs_rt_input
