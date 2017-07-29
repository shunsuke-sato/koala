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
module inputoutput_list
  implicit none

  integer,parameter :: nlen_filename = 256
! input file
  integer,parameter :: id_inputfile = 21
  character(nlen_filename) :: name_inputfile = './input'
! input_log file
  integer,parameter :: id_input_log = 22
  character(nlen_filename) :: name_input_log = './input_log.out'


!! temporal files
  integer,parameter :: id_ps_file = 101

!-------------------------------------------------------------------------------
end module inputoutput_list
