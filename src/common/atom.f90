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
module atom
  use global_variables
  use communication
  use parallel
  use inputoutput_list
  implicit none
  private

  integer,parameter :: max_species = 20

  public :: read_species

contains
!-------------------------------------------------------------------------------
  subroutine read_species
    implicit none
    logical :: if_found, if_error
    character(256) :: char_t
    integer :: inum,istat

    if(if_root_global)then
      rewind(id_inputfile)
      if_found = .false.
      if_error = .false.

      do
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)exit
        if(trim(adjustl(char_t)) == '%species')then
          if_found = .true.
          exit
        end if
      end do

      inum = 0
      do 
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)then
           if_error = .true.
           exit
        end if
        if(trim(adjustl(char_t)) == '/')then
          exit
        else
          inum = inum + 1
        end if
      end do
      num_element = inum

    end if

    call comm_bcast(num_element)
    if(num_element == 0)call error_finalize("Error: %species should be specified in input.")
    call comm_bcast(if_found)
    if(.not.if_found)call error_finalize("Error: %species should be specified in input.")
    call comm_bcast(if_error)
    if(if_error)call error_finalize("Error: %species should be closed by slash (/).")

  end subroutine read_species

!-------------------------------------------------------------------------------
end module atom
