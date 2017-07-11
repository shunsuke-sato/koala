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
subroutine comm_bcast_integer(a, communicator, root)
  implicit none
  integer,intent(inout) :: a
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, 1, MPI_INTEGER, id_root, id_comm, ierr)

end subroutine comm_bcast_integer
!-------------------------------------------------------------------------------
subroutine comm_bcast_integer_1d(a, communicator, root)
  implicit none
  integer,intent(inout) :: a(:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr

  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_INTEGER, id_root, id_comm, ierr)

end subroutine comm_bcast_integer_1d
!-------------------------------------------------------------------------------
subroutine comm_bcast_integer_2d(a, communicator, root)
  implicit none
  integer,intent(inout) :: a(:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_INTEGER, id_root, id_comm, ierr)
  
end subroutine comm_bcast_integer_2d
!-------------------------------------------------------------------------------
subroutine comm_bcast_integer_3d(a, communicator, root)
  implicit none
  integer,intent(inout) :: a(:,:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_INTEGER, id_root, id_comm, ierr)

end subroutine comm_bcast_integer_3d
!-------------------------------------------------------------------------------
subroutine comm_bcast_real8(a, communicator, root)
  implicit none
  real(8),intent(inout) :: a
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, 1, MPI_DOUBLE_PRECISION, id_root, id_comm, ierr)

end subroutine comm_bcast_real8
!-------------------------------------------------------------------------------
subroutine comm_bcast_real8_1d(a, communicator, root)
  implicit none
  real(8),intent(inout) :: a(:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_PRECISION, id_root, id_comm, ierr)
  
end subroutine comm_bcast_real8_1d
!-------------------------------------------------------------------------------
subroutine comm_bcast_real8_2d(a, communicator, root)
  implicit none
  real(8),intent(inout) :: a(:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_PRECISION, id_root, id_comm, ierr)

end subroutine comm_bcast_real8_2d
!-------------------------------------------------------------------------------
subroutine comm_bcast_real8_3d(a, communicator, root)
  implicit none
  real(8),intent(inout) :: a(:,:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_PRECISION, id_root, id_comm, ierr)
  
end subroutine comm_bcast_real8_3d
!-------------------------------------------------------------------------------
subroutine comm_bcast_real8_4d(a, communicator, root)
  implicit none
  real(8),intent(inout) :: a(:,:,:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_PRECISION, id_root, id_comm, ierr)
  
end subroutine comm_bcast_real8_4d
!-------------------------------------------------------------------------------
subroutine comm_bcast_complex8(a, communicator, root)
  implicit none
  complex(8),intent(inout) :: a
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, 1, MPI_DOUBLE_COMPLEX, id_root, id_comm, ierr)
  
end subroutine comm_bcast_complex8
!-------------------------------------------------------------------------------
subroutine comm_bcast_complex8_1d(a, communicator, root)
  implicit none
  complex(8),intent(inout) :: a(:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_COMPLEX, id_root, id_comm, ierr)
  
end subroutine comm_bcast_complex8_1d
!-------------------------------------------------------------------------------
subroutine comm_bcast_complex8_2d(a, communicator, root)
  implicit none
  complex(8),intent(inout) :: a(:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_COMPLEX, id_root, id_comm, ierr)
  
end subroutine comm_bcast_complex8_2d
!-------------------------------------------------------------------------------
subroutine comm_bcast_complex8_3d(a, communicator, root)
  implicit none
  complex(8),intent(inout) :: a(:,:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_COMPLEX, id_root, id_comm, ierr)
  
end subroutine comm_bcast_complex8_3d
!-------------------------------------------------------------------------------
subroutine comm_bcast_complex8_4d(a, communicator, root)
  implicit none
  complex(8),intent(inout) :: a(:,:,:,:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a), MPI_DOUBLE_COMPLEX, id_root, id_comm, ierr)
  
end subroutine comm_bcast_complex8_4d
!-------------------------------------------------------------------------------
subroutine comm_bcast_character(a, communicator, root)
  implicit none
  character(*),intent(inout) :: a
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, len(a), MPI_CHARACTER, id_root, id_comm, ierr)
  
end subroutine comm_bcast_character
!-------------------------------------------------------------------------------
subroutine comm_bcast_character_1d(a, communicator, root)
  implicit none
  character(*),intent(inout) :: a(:)
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, size(a)*len(a), MPI_CHARACTER, id_root, id_comm, ierr)
  
end subroutine comm_bcast_character_1d
!-------------------------------------------------------------------------------
subroutine comm_bcast_logical(a, communicator, root)
  implicit none
  logical,intent(inout) :: a
  integer,intent(in),optional :: communicator
  integer,intent(in),optional :: root
  integer :: id_comm, id_root, ierr
  
  id_comm = communicator_check(present(communicator),communicator)
  id_root = root_check(present(root),root)
  call MPI_Bcast(a, 1, MPI_LOGICAL, id_root, id_comm, ierr)
  
end subroutine comm_bcast_logical
!-------------------------------------------------------------------------------
