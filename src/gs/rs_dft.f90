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
  use read_gs_rt_input
  implicit none

!  private

  public :: gs_rs_dft

contains
!-------------------------------------------------------------------------------
  subroutine gs_rs_dft
    implicit none

    call read_common_input_for_rtrs_tddft

  end subroutine gs_rs_dft

!-------------------------------------------------------------------------------
end module rs_dft
