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
module fft
  implicit none
  include 'fftw3.f'
  private


  integer :: n1, n2, n3

  real(8) :: plan_3d_complex_forward
  real(8) :: plan_3d_complex_backward
  complex(8),allocatable :: rs_3d_complex(:,:,:)
  complex(8),allocatable :: fs_3d_complex(:,:,:)

  real(8) :: plan_3d_real_forward
  real(8) :: plan_3d_real_backward
  real(8),allocatable :: rs_3d_real(:,:,:)
  complex(8),allocatable :: fs_3d_real(:,:,:)

  public :: fft3d_initialize

  contains
!-------------------------------------------------------------------------------
    subroutine fft3d_initialize(l,m,n)
      implicit none
      integer,intent(in) :: l,m,n

      n1 = l
      n2 = m
      n3 = n

      allocate(rs_3d_complex(n1,n2,n3))
      allocate(fs_3d_complex(n1,n2,n3))

      call dfftw_plan_dft_3d(plan_3d_complex_forward, n1,n2,n3, &
                            &rs_3d_complex,fs_3d_complex, &
                            &FFTW_FORWARD, FFTW_MEASURE)

      call dfftw_plan_dft_3d(plan_3d_complex_backward, n1,n2,n3, &
                            &fs_3d_complex,rs_3d_complex, &
                            &FFTW_BACKWARD, FFTW_MEASURE)

    end subroutine fft3d_initialize
!-------------------------------------------------------------------------------
!    subroutine fft3d_complex_forward(in,out)
!      implicit none
!      complex(8),intent(in) :: in(n1,n2,n3)
!      complex(8),intent(out) :: out(n1,n2,n3)
!
!      rs_3d_complex = in
!      call dfftw_execute(plan_3d_complex_forward)
!      out = fs_3d_complex
!
!    end subroutine fft3d_complex_forward
!!-------------------------------------------------------------------------------
!    subroutine fft3d_complex_backward(in,out)
!      implicit none
!      complex(8),intent(in) :: in(n1,n2,n3)
!      complex(8),intent(out) :: out(n1,n2,n3)
!
!      fs_3d_complex = in
!      call dfftw_execute(plan_3d_complex_backward)
!      out = rs_3d_complex
!
!    end subroutine fft3d_complex_backward
!-------------------------------------------------------------------------------

end module fft
