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

  write(*,*)"calc_mode",comm_id_global,trim(calc_mode)
  write(*,"(A,I5,3e16.6e3)")"a_vec(:,1)",comm_id_global,a_cvec(:,1)
  write(*,"(A,I5,3e16.6e3)")"a_vec(:,2)",comm_id_global,a_cvec(:,2)
  write(*,"(A,I5,3e16.6e3)")"a_vec(:,3)",comm_id_global,a_cvec(:,3)
  write(*,*)"nl",comm_id_global,nl
  write(*,*)"nt",comm_id_global,nt
  write(*,*)"dt",comm_id_global,dt

  call fin_parallel

end program main
