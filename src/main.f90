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
program main
  use global_variables
  use parallel
  use communication
  use input
  use rs_dft
  use rtrs_tddft
  implicit none
  integer :: i

  call init_parallel
  call init_input
  call read_basic_input('calc_mode',calc_mode,val_default='none')

  select case(calc_mode)
  case('gs')
    call gs_rs_dft
  case('rt')
    call tdrun_rtrs_tddft
  case default
  end select

  call fin_input
  call fin_parallel

end program main
