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
program main
  use global_variables
  use parallel
  use communication
  use input
  implicit none
  integer :: i

  call init_parallel
  call init_input
  call read_basic_input('calc_mode',calc_mode,'none')
  call read_basic_input('nt',Nt,0)
  call read_basic_input('dt',dt,0.0d0)

  write(*,*)"calc_mode",comm_id_global,trim(calc_mode)
  write(*,*)"nt",comm_id_global,nt
  write(*,*)"dt",comm_id_global,dt

  write(*,*)"myrank",comm_id_global
  i = comm_id_global
  call comm_bcast(i)
  write(*,*)"i",i

  i = comm_id_global
  call comm_bcast(i,communicator=comm_group_global)
  write(*,*)"i-comm,0",i

  i = comm_id_global
  call comm_bcast(i,root=1)
  write(*,*)"i-root,1",i

  call fin_parallel

end program main
