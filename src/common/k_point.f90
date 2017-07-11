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
module k_point
  use global_variables
  use parallel
  implicit none

  private
  public :: init_k_grid_3d

contains
!-------------------------------------------------------------------------------
  subroutine init_k_grid_3d
    implicit none
    integer :: i,i1,i2,i3

    if(if_root_global)then
       write(*,"(A)")"Start: Initialize k-point grids."
    end if

    nk_tot = nk(1)*nk(2)*nk(3)
    dk(:) = 1d0/dble(nk(:))

    allocate(kac_rvec(3,nk_tot), kac0_rvec(3,nk_tot))
    allocate(kac_Cvec(3,nk_tot), kac0_cvec(3,nk_tot))


    i=0
    do i1=0,nk(1)-1
       do i2=0,nk(2)-1
          do i3=0,nk(3)-1
             i=i+1
             kac0_rvec(1,i)=dble(i1)*dk(1)-dk(1)*(dble(nk(1)/2)-0.5d0)
             kac0_rvec(2,i)=dble(i2)*dk(2)-dk(2)*(dble(nk(2)/2)-0.5d0)
             kac0_rvec(3,i)=dble(i3)*dk(3)-dk(3)*(dble(nk(3)/2)-0.5d0)
          end do
       end do
    end do
    
    kac0_cvec = matmul(b_cvec,kac0_rvec)

    if(if_root_global)then
       write(*,"(A)")"End: Initialize k-point grids."
    end if

  end subroutine init_k_grid_3d
!-------------------------------------------------------------------------------
end module k_point
