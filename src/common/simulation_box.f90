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
module simulation_box
  use global_variables
  use parallel
  use fft
  implicit none

  private
  public :: init_simul_box

contains
!-------------------------------------------------------------------------------
  subroutine init_simul_box
    implicit none
    real(8) :: tvec(3)
    integer :: i,i1,i2,i3

    if(if_root_global)write(*,"(A)")"Started: Initialization of simulation box."

    nl_tot = nl(1)*nl(2)*nl(3)
    hl(:) = 1d0/dble(nl(:))

! rotation of lattice vectors
    tvec(1)=a_cvec(2,2)*a_cvec(3,3)-a_cvec(3,2)*a_cvec(2,3)
    tvec(2)=a_cvec(3,2)*a_cvec(1,3)-a_cvec(1,2)*a_cvec(3,3)
    tvec(3)=a_cvec(1,2)*a_cvec(2,3)-a_cvec(2,2)*a_cvec(1,3)

    vcell=abs(sum(a_cvec(:,1)*tvec(:)))
    dvcell=vcell/dble(nl_tot)

! reciprocal lattice vectors
    b_Cvec(1,1)=2d0*pi*(a_Cvec(2,2)*a_Cvec(3,3)-a_Cvec(3,2)*a_Cvec(2,3))
    b_Cvec(2,1)=2d0*pi*(a_Cvec(3,2)*a_Cvec(1,3)-a_Cvec(1,2)*a_Cvec(3,3))
    b_Cvec(3,1)=2d0*pi*(a_Cvec(1,2)*a_Cvec(2,3)-a_Cvec(2,2)*a_Cvec(1,3))

    b_Cvec(1,2)=2d0*pi*(a_Cvec(2,3)*a_Cvec(3,1)-a_Cvec(3,3)*a_Cvec(2,1))
    b_Cvec(2,2)=2d0*pi*(a_Cvec(3,3)*a_Cvec(1,1)-a_Cvec(1,3)*a_Cvec(3,1))
    b_Cvec(3,2)=2d0*pi*(a_Cvec(1,3)*a_Cvec(2,1)-a_Cvec(2,3)*a_Cvec(1,1))

    b_Cvec(1,3)=2d0*pi*(a_Cvec(2,1)*a_Cvec(3,2)-a_Cvec(3,1)*a_Cvec(2,2))
    b_Cvec(2,3)=2d0*pi*(a_Cvec(3,1)*a_Cvec(1,2)-a_Cvec(1,1)*a_Cvec(3,2))
    b_Cvec(3,3)=2d0*pi*(a_Cvec(1,1)*a_Cvec(2,2)-a_Cvec(2,1)*a_Cvec(1,2))

    b_Cvec=b_Cvec/sum(a_Cvec(:,1)*tvec(:))
    
    allocate(ilx(3,nl_tot), ilx123(0:nl(1)-1,0:nl(2)-1,0:nl(3)-1) )
    allocate(lx(3,nl_tot) )
    i=0
    do i1=0,nl(1)-1
      do i2=0,nl(2)-1
        do i3=0,nl(3)-1
          i=i+1
          ilx(1,i)=i1; ilx(2,i)=i2; ilx(3,i)=i3
          ilx123(i1,i2,i3)=i
        end do
      end do
    end do
    lx(1,:)=dble(ilx(1,:))*hl(1)
    lx(2,:)=dble(ilx(2,:))*hl(2)
    lx(3,:)=dble(ilx(3,:))*hl(3)

    call fft3d_initialize(nl(1),nl(2),nl(3))

    if(if_root_global)write(*,"(A)")"Finished: Initialization of simulation box."

  end subroutine init_simul_box
!-------------------------------------------------------------------------------
end module simulation_box
