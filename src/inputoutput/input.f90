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
module input
  use global_variables
  use parallel
  use communication
  use inputoutput_list
  implicit none

  private
  integer,parameter :: len_max = 256

  public :: init_input, &
            read_basic_input
!            read_calc_mode

  interface read_basic_input
     module procedure read_basic_input_character
!     module procedure read_basic_input_integer
!     module procedure read_basic_input_real8
  end interface read_basic_input

contains
!-------------------------------------------------------------------------------
  subroutine init_input
    implicit none

    if(if_root_global)then
      open(id_inputfile,file=name_inputfile)
    end if

  end subroutine init_input
!-------------------------------------------------------------------------------
  subroutine read_basic_input_character(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    character(*),intent(out) :: val
    character(*),intent(in) :: val_default
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    
    if(if_root_global)then
      val = trim(val_default)
      call lookup_input(name, if_found, val_t)
      if(if_found)then
        index_equal= index(val_t,'=')
        length_trimed = len_trim(val_t)
        val = trim(val_t(index_equal+1:length_trimed))
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_basic_input_character
!-------------------------------------------------------------------------------
  subroutine lookup_input(char_in, if_found, char_out)
    implicit none
    character(*),intent(in) :: char_in
    logical,intent(out) :: if_found
    character(len_max),intent(out),optional :: char_out
    character(len_max) :: char_t
    integer :: istat, len_t

    if(if_root_global)then
      if_found = .false.
      len_t = len(char_in)
      rewind(id_inputfile)

      do 
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)exit
        if(char_in == char_t(1:len_t))then
          if_found = .true.
          if(present(char_out))char_out = char_t
        end if

      end do

    end if

  end subroutine lookup_input


!-------------------------------------------------------------------------------
end module input
