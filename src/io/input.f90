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
module input
  use global_variables
  use parallel
  use communication
  use inputoutput_list
  implicit none

  private
  integer,parameter :: len_max = 256
  integer,parameter :: max_species = 20

  public :: init_input, &
            read_basic_input, &
            read_vector_input, &
            read_matrix_input, &
            read_common_input, &
            read_species, &
            read_atomic_coordinates, &
            fin_input


  interface read_basic_input
     module procedure read_basic_input_character
     module procedure read_basic_input_integer
     module procedure read_basic_input_real8
  end interface read_basic_input

  interface read_vector_input
     module procedure read_vector_input_integer
     module procedure read_vector_input_real8
  end interface read_vector_input

  interface read_matrix_input
     module procedure read_matrix_input_integer
     module procedure read_matrix_input_real8
  end interface

contains
!-------------------------------------------------------------------------------
  subroutine init_input
    implicit none

    if(if_root_global)then
      open(id_inputfile,file=name_inputfile)
      open(id_input_log,file=name_input_log)
    end if

  end subroutine init_input
!-------------------------------------------------------------------------------
  subroutine fin_input
    implicit none

    if(if_root_global)then
      close(id_inputfile)
      close(id_input_log)
    end if

  end subroutine fin_input
!-------------------------------------------------------------------------------
  subroutine read_common_input
    implicit none

    logical :: if_default

! real-space
    call read_vector_input('%num_rs_grid',nl,val_default=(/-1,-1,-1/))
    if(if_root_global)write(id_input_log,"(A,3i7)")'%num_rs_grid = ',nl

    call read_vector_input('%lattice_parameter',al,val_default=(/1d0,1d0,1d0/))
    if(if_root_global)write(id_input_log,"(A,3e26.16e3)")'%lattice_parameter = ',al

    call read_matrix_input('%lattice_vector_pre',a_pre_cvec, &
         val_default = reshape( (/1d0,0d0,0d0,0d0,1d0,0d0,0d0,0d0,1d0/), (/3,3/)) )
    if(if_root_global)then
      write(id_input_log,"(A)")'%lattice_vector_pre'
      write(id_input_log,"(3e26.16e3)")a_pre_cvec(1,:)
      write(id_input_log,"(3e26.16e3)")a_pre_cvec(2,:)
      write(id_input_log,"(3e26.16e3)")a_pre_cvec(3,:)
      write(id_input_log,"(A)")'/'
    end if

    a_pre_cvec = transpose(a_pre_cvec)
    a_cvec(:,1) = al(1)*a_pre_cvec(:,1)
    a_cvec(:,2) = al(2)*a_pre_cvec(:,2)
    a_cvec(:,3) = al(3)*a_pre_cvec(:,3)

! reciprocal-lattice spacew
    call read_vector_input('%num_k_grid',nk,val_default=(/-1,-1,-1/))
    if(if_root_global)write(id_input_log,"(A,3i7)")'%num_k_grid = ',nk

! material
    call read_basic_input('num_elec',num_elec,val_default=-1)
    if(if_root_global)write(id_input_log,"(A,i7)")'num_elec = ',num_elec

    call read_basic_input('num_band',num_band,val_default=num_elec)
    if(if_root_global)write(id_input_log,"(A,i7)")'num_band = ',num_band

    call read_basic_input('nt',Nt,val_default=0)
    if(if_root_global)write(id_input_log,"(A,i7)")'nt = ',nt

    call read_basic_input('dt',dt,val_default=0.0d0)
    if(if_root_global)write(id_input_log,"(A,e26.16e3)")'dt = ',dt


  end subroutine read_common_input
!-------------------------------------------------------------------------------
  subroutine read_species
    implicit none
    logical :: if_found, if_error
    character(256) :: char_t
    integer :: inum,istat,ielem,illoc

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

    allocate(lloc_ps(num_element))
    allocate(ps_file(num_element))


    if(if_root_global)then
      rewind(id_inputfile)
      if_error = .false.

      do
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)exit
        if(trim(adjustl(char_t)) == '%species')then
          if_found = .true.
          exit
        end if
      end do

      do inum = 1, num_element
        read(id_inputfile, *)ielem,char_t,illoc
        ps_file(ielem) = trim(char_t)
        lloc_ps(ielem) = lloc_ps(ielem)
        if(ielem>num_element)if_error=.true.
      end do

    end if
    call comm_bcast(if_error)
    if(if_error)call error_finalize("Error: serial number for spieces is wrong.")
    call comm_bcast(ps_file)
    call comm_bcast(lloc_ps)

    if(if_root_global)then
      write(id_input_log,"(A)")'%species'
      do ielem = 1, num_element
         write(id_input_log,"(i7,2x,A,i7)")ielem,trim(ps_file(ielem)),lloc_ps(ielem)
      end do
      write(id_input_log,"(A)")'/'
    end if


  end subroutine read_species
!-------------------------------------------------------------------------------
  subroutine read_atomic_coordinates
    implicit none
    logical :: if_error, if_found
    integer :: inum,ielem,istat
    real(8) :: rvec(3)
    character(256) :: char_t    

    if(if_root_global)then
      rewind(id_inputfile)
      if_found = .false.
      if_error = .false.

      do
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)exit
        if(trim(adjustl(char_t)) == '%atomic_coor_red')then
          if_found = .true.
          exit
        end if
      end do

      if(.not.if_found)if_error = .true.
      
      if(.not.if_error)then
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
         num_ion = inum

      end if
    end if

    call comm_bcast(if_found)
    if(.not.if_found)call error_finalize("Error: %atomic_coor_red should be specified in input.")
    call comm_bcast(num_ion)
    allocate(Rion_cvec(3,num_ion),Rion_rvec(3,num_ion),kion(num_ion))


    if(if_root_global)then
      rewind(id_inputfile)

      do
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)exit
        if(trim(adjustl(char_t)) == '%atomic_coor_red')then
          if_found = .true.
          exit
        end if
      end do

      do inum = 1, num_ion
        read(id_inputfile, *)ielem,rvec(:)
        kion(inum) = ielem
        rion_rvec(:,inum) = rvec(:)
      end do

    end if

    call comm_bcast(rion_rvec)
    call comm_bcast(kion)

    if(if_root_global)then
      write(id_input_log,"(A)")'%atomic_coor_red'
      do inum = 1, num_ion
         write(id_input_log,"(i7,2x,3e26.16e3)")kion(inum),rion_rvec(:,inum)
      end do
      write(id_input_log,"(A)")'/'
    end if
    

  end subroutine read_atomic_coordinates
!-------------------------------------------------------------------------------
  subroutine read_basic_input_character(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    character(*),intent(out) :: val
    character(*),intent(in),optional :: val_default
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    
    if(if_root_global)then
      if(present(val_default))val = val_default
      call lookup_input(name, if_found, val_t)
      if(if_found)then
        index_equal= index(val_t,'=')
        length_trimed = len_trim(val_t)
        val = trim(adjustl(val_t(index_equal+1:length_trimed)))
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_basic_input_character
!-------------------------------------------------------------------------------
  subroutine read_basic_input_integer(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    integer,intent(out) :: val
    integer,intent(in),optional :: val_default
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    
    if(if_root_global)then
      if(present(val_default))val = val_default
      call lookup_input(name, if_found, val_t)
      if(if_found)then
        index_equal= index(val_t,'=')
        length_trimed = len_trim(val_t)
        val_t = trim(val_t(index_equal+1:length_trimed))
        read(val_t,*) val
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_basic_input_integer
!-------------------------------------------------------------------------------
  subroutine read_basic_input_real8(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    real(8),intent(out) :: val
    real(8),intent(in),optional :: val_default
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    
    if(if_root_global)then
      if(present(val_default))val = val_default
      call lookup_input(name, if_found, val_t)
      if(if_found)then
        index_equal= index(val_t,'=')
        length_trimed = len_trim(val_t)
        val_t = trim(val_t(index_equal+1:length_trimed))
        read(val_t,*) val
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_basic_input_real8
!-------------------------------------------------------------------------------
  subroutine read_vector_input_integer(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    integer,intent(out) :: val(:)
    integer,intent(in),optional :: val_default(:)
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    
    if(if_root_global)then
      if(present(val_default))val = val_default
      call lookup_input(name, if_found)
      if(if_found)then
        read(id_inputfile, *) val(:)
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_vector_input_integer
!-------------------------------------------------------------------------------
  subroutine read_vector_input_real8(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    real(8),intent(out) :: val(:)
    real(8),intent(in),optional :: val_default(:)
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    
    if(if_root_global)then
      if(present(val_default))val = val_default
      call lookup_input(name, if_found)
      if(if_found)then
        read(id_inputfile, *) val(:)
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_vector_input_real8
!-------------------------------------------------------------------------------
  subroutine read_matrix_input_integer(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    integer,intent(out) :: val(:,:)
    integer,intent(in),optional :: val_default(:,:)
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    integer :: i_lbound, i_ubound, i
    
    if(if_root_global)then
      i_lbound = lbound(val,1); i_ubound = ubound(val,1)

      if(present(val_default))val = val_default
      call lookup_input(name, if_found)
      if(if_found)then
        do i = i_lbound, i_ubound
          read(id_inputfile, *) val(i,:)
        end do
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_matrix_input_integer
!-------------------------------------------------------------------------------
  subroutine read_matrix_input_real8(name, val, val_default, if_default)
    implicit none
    character(*),intent(in) :: name
    real(8),intent(out) :: val(:,:)
    real(8),intent(in),optional :: val_default(:,:)
    logical,intent(out),optional :: if_default
    character(len_max) :: val_t
    logical :: if_found
    integer :: index_equal, length_trimed
    integer :: i_lbound, i_ubound, i
    
    if(if_root_global)then
      i_lbound = lbound(val,1); i_ubound = ubound(val,1)

      if(present(val_default))val = val_default
      call lookup_input(name, if_found)
      if(if_found)then
        do i = i_lbound, i_ubound
          read(id_inputfile, *) val(i,:)
        end do
      end if
    end if
    call comm_bcast(val)

    if(present(if_default))then
      if_default = .not.if_found
      call comm_bcast(if_default)
    end if

  end subroutine read_matrix_input_real8

!-------------------------------------------------------------------------------
  subroutine lookup_input(char_in, if_found, char_out)
    implicit none
    character(*),intent(in) :: char_in
    logical,intent(out) :: if_found
    character(len_max),intent(out),optional :: char_out
    character(len_max) :: char_t
    integer :: istat, len_t, index_equal

    if(if_root_global)then
      if_found = .false.
      len_t = len(char_in)
      rewind(id_inputfile)

      do 
        read(id_inputfile, '(a)', iostat=istat) char_t
        if(istat < 0)exit
        index_equal = index(char_t,'=')
        if(index_equal == 0)then
          if(char_in == trim(char_t))then
            if_found = .true.
            if(present(char_out))char_out = char_t
            return
          end if
        else
          if(char_in == trim(char_t(1:index_equal-1)))then
            if_found = .true.
            if(present(char_out))char_out = char_t
            return
          end if
        end if

      end do

    end if

  end subroutine lookup_input


!-------------------------------------------------------------------------------
end module input
