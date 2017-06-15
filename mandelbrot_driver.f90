PROGRAM mandelbrot_driver

    ! Get kind parameters for long integers and double precision
    USE ISO_FORTRAN_ENV, ONLY: INT32, REAL64

    IMPLICIT NONE

    ! Parameters (image size, convergence criteria, window size)
    INTEGER(kind=INT32), PARAMETER :: nx = 400, ny = 400
    INTEGER(kind=INT32), PARAMETER :: maxiter = 100
    REAL(kind=REAL64), PARAMETER :: cut = 4.0d0
    REAL(kind=REAL64), PARAMETER :: xmin = -2.2, xmax = 1.0
    REAL(kind=REAL64), PARAMETER :: ymin = -1.2, ymax = 1.2

    ! Work variables
    REAL(kind=REAL64) :: window(4)
    REAL(kind=REAL64), ALLOCATABLE :: magnitude(:,:)

    ! Use array constructor to set window size
    window = (/xmin, ymin, xmax, ymax/)

    ALLOCATE(magnitude(nx, ny))

    CALL mandelbrot(nx, ny, maxiter, window, cut, magnitude)

    ! Check if kind is always equivalent to number of bytes
    OPEN(unit=10, file='mandelbrot.out', status='REPLACE', access='DIRECT', &
         & recl=nx*ny*STORAGE_SIZE(magnitude)/8, &
         & form='UNFORMATTED')
    WRITE(10, rec=1) magnitude
    CLOSE(10)

    DEALLOCATE(magnitude)

END PROGRAM mandelbrot_driver
