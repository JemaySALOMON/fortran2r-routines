! Returns the inverse of a matrix calculated by finding the LU
! decomposition.  Depends on LAPACK.
!for source code, see Fortran Wiki (https://fortranwiki.org/fortran/show/Matrix+inversion)
! I did small modifs to call it with some errors message in R directly, but you may build an Rcpp wrapper for use in R package

subroutine inv(n,A, Ainv, info, msg) bind(C, name="solvef90")
  use iso_c_binding, only: c_char, c_ptr
  integer, parameter :: dp = kind(1.0d0)
  integer :: n, info                               !  <-- (2) n passed in from R
  real(dp), dimension(n,n), intent(in)  :: A  !  <-- (3) explicit shape, not (:,:)
  real(dp), dimension(n,n) :: Ainv

  real(dp), dimension(n) :: work  ! work array for LAPACK
  integer, dimension(n) :: ipiv   ! pivot indices

   type(c_ptr), intent(in) :: msg        !for error message char* buffer supplied by R

  ! External procedures defined in LAPACK
  external DGETRF
  external DGETRI

  ! Store A in Ainv to prevent it from being overwritten by LAPACK
  Ainv = A
  n = size(A,1)

  ! DGETRF computes an LU factorization of a general M-by-N matrix A
  ! using partial pivoting with row interchanges.
  call DGETRF(n, n, Ainv, n, ipiv, info)

  if (info /= 0) then
     call set_msg(msg, 'Matrix is numerically singular!')
  end if

  ! DGETRI computes the inverse of a matrix using the LU factorization
  ! computed by DGETRF.
  call DGETRI(n, Ainv, n, ipiv, work, n, info)

  if (info /= 0) then
    call set_msg(msg, 'Matrix inversion failed!')
 end if

 call set_msg(msg, '') 
end subroutine inv


! Fortran string into R's C char buffer (null-terminated).
! usefull for error message instead of crashing the R session
subroutine set_msg(cbuf, text)
  use iso_c_binding, only: c_char, c_ptr, c_f_pointer, c_null_char
  type(c_ptr), intent(in) :: cbuf
  character(*), intent(in) :: text
  character(kind=c_char), pointer :: s(:)
  integer :: i, L
  L = len_trim(text)
  call c_f_pointer(cbuf, s, [L+1])      ! view the C buffer as a char array
  do i = 1, L
     s(i) = text(i:i)
  end do
  s(L+1) = c_null_char
end subroutine set_msg
