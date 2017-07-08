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
module parallel
  use mpi
  implicit none

  private
! MPI global
  integer, public :: nproc_global, &
                     myrank_global
  logical, public :: if_root_global
                     
! OMP
  integer, public :: nthread_omp

  public :: init_parallel

contains
!-------------------------------------------------------------------------------
  subroutine init_parallel
    implicit none
    integer :: ierr
!$ integer :: omp_get_max_threads  

    call MPI_init(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc_global,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myrank_global,ierr)

    if(myrank_global == 0)then
       if_root_global = .true.
    else
       if_root_global = .false.
    end if

    nthread_omp = 1
!$  nthread_omp=omp_get_max_threads()

  end subroutine init_parallel

end module parallel
