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
module communication
  use mpi
  use parallel
  implicit none

  private


  public :: comm_bcast

  interface comm_bcast
    module procedure comm_bcast_integer
    module procedure comm_bcast_integer_1d
    module procedure comm_bcast_integer_2d
    module procedure comm_bcast_integer_3d
    module procedure comm_bcast_real8
    module procedure comm_bcast_real8_1d
    module procedure comm_bcast_real8_2d
    module procedure comm_bcast_real8_3d
    module procedure comm_bcast_real8_4d
    module procedure comm_bcast_complex8
    module procedure comm_bcast_complex8_1d
    module procedure comm_bcast_complex8_2d
    module procedure comm_bcast_complex8_3d
    module procedure comm_bcast_complex8_4d
    module procedure comm_bcast_character
    module procedure comm_bcast_character_1d
    module procedure comm_bcast_logical
 end interface comm_bcast

contains
!-------------------------------------------------------------------------------
  function int_switch(if_true, int_true, int_false) result(int_result)
    implicit none
    logical,intent(in) :: if_true
    integer,intent(in) :: int_true, int_false
    integer :: int_result

    if(if_true)then
      int_result = int_true
    else
      int_result = int_false
   end if
  end function int_switch
!-------------------------------------------------------------------------------
  include "include/comm_bcast.f90"
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
end module communication
