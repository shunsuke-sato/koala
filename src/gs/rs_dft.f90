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
module rs_dft
  use global_variables
  use parallel
  use communication
  use simulation_box
  use k_point
  use orbital
  use input
  implicit none

!  private

  public :: gs_rs_dft
        

contains
!-------------------------------------------------------------------------------
  subroutine gs_rs_dft
    implicit none

    call read_common_input
    call read_species
    call read_atomic_coordinates
    call init_simul_box
    call init_k_grid_3d
    call init_orbital
    call init_occupation

    call init_gs_array


  end subroutine gs_rs_dft
!-------------------------------------------------------------------------------
  subroutine init_gs_array
    implicit none

    allocate(zwfn_gs(0:nl(1)-1, 0:nl(2)-1, 0:nl(3)-1, num_orb))
    allocate(zwfn_gs0(0:nl(1)-1, 0:nl(2)-1, 0:nl(3)-1, num_orb))
    allocate(rho(0:nl(1)-1, 0:nl(2)-1, 0:nl(3)-1))
    allocate(vloc(0:nl(1)-1, 0:nl(2)-1, 0:nl(3)-1))
    allocate(vh(0:nl(1)-1, 0:nl(2)-1, 0:nl(3)-1))
    allocate(vxc(0:nl(1)-1, 0:nl(2)-1, 0:nl(3)-1))

  end subroutine init_gs_array
!-------------------------------------------------------------------------------
end module rs_dft
