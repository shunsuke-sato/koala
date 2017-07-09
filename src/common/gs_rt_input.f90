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
module gs_rt_input
  use global_variables
  use input
  implicit none

  private
  public :: read_common_input_for_rtrs_tddft

contains
!-------------------------------------------------------------------------------
  subroutine read_common_input_for_rtrs_tddft
    implicit none

! real-space
    call read_vector_input('%num_rs_grid',nl,(/1,1,1/))

    call read_basic_input('nt',Nt,0)
    call read_basic_input('dt',dt,0.0d0)

  end subroutine read_common_input_for_rtrs_tddft

!-------------------------------------------------------------------------------
end module gs_rt_input
