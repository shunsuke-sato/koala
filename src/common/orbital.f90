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
module orbital
  use global_variables
  implicit none

  private
  public :: init_orbital, &
             init_occupation

contains
!-------------------------------------------------------------------------------
  subroutine init_orbital
    implicit none
    integer :: iorb, ik, ib

    num_orb = nk_tot * num_band
    allocate(list_kpoint(num_orb), list_band(num_orb))
    allocate(list_orb2(num_band, nk_tot))

    iorb = 0
    do ik = 1, nk_tot
      do ib = 1, num_band
        iorb = iorb + 1
        list_kpoint(iorb) = ik
        list_band(iorb) = ib
        list_orb2(ib,ik) = iorb
      end do
    end do

  end subroutine init_orbital
!-------------------------------------------------------------------------------
  subroutine init_occupation
    implicit none

    allocate(occ(num_band,nk_tot))
    occ = 0d0
    occ(1:num_elec/2, nk_tot) = 2d0/dble(nk_tot)

  end subroutine init_occupation
!-------------------------------------------------------------------------------
end module orbital
