SUBROUTINE mandelbrot(nx, ny, maxiter, window, cut, magnitude)

    ! Get kind parameters for long integers and double precision
    USE ISO_FORTRAN_ENV, ONLY: INT32, REAL64

    IMPLICIT NONE

    ! Dummy arguments
    INTEGER(kind=INT32), INTENT(in) :: nx, ny, maxiter
    REAL(kind=REAL64), INTENT(in) :: window(4), cut
    REAL(kind=REAL64), INTENT(out) :: magnitude(nx, ny)

    ! Work variables
    INTEGER(kind=INT32) :: i, j, k
    REAL(kind=REAL64) :: start, span, xaxis(nx), yaxis(ny), abs_z
    COMPLEX(kind=REAL64) :: c, z

    ! Set axes, requires type conversion integer to real
    start = window(1)
    span = window(3) - window(1)
    xaxis = start + span*REAL((/(i, i = 1, nx)/)-1, kind=REAL64)/REAL(nx-1, kind=REAL64)

    start = window(2)
    span = window(4) - window(2)
    yaxis = start + span*REAL((/(i, i = 1, ny)/)-1, kind=REAL64)/REAL(ny-1, kind=REAL64)

    ! Compute Mandelbrot function for each element of the set and store its magnitude
    DO j = 1, ny
        DO i = 1, nx
           c = CMPLX(xaxis(i), yaxis(j))
           z = CMPLX(0.0_REAL64, 0.0_REAL64)
           DO k = 1, maxiter
              abs_z = ABS(z)
              IF (abs_z > cut) EXIT
              z = z*z+c
           END DO
           magnitude(i,j) = abs_z
        END DO
    END DO

END SUBROUTINE mandelbrot