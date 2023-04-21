# Fortran-Codes
! convergence_measure.f90
program convergence_measure
    implicit none
    integer, parameter :: n_iterations = 10
    integer :: i
    real :: x0, x1, x, y0, y1, y, y_prev, abs_error

    ! Input x0, x1, y0, y1, and the desired x value for interpolation
    write (*,*) "Enter x0, x1, y0, and y1 (separated by spaces):"
    read (*,*) x0, x1, y0, y1
    write (*,*) "Enter the x value for interpolation:"
    read (*,*) x

    ! Initialize previous approximation and perform the first iteration
    y_prev = y0 + (x - x0) * (y1 - y0) / (x1 - x0)
    do i = 1, n_iterations
        ! Linear interpolation
        y = y0 + (x - x0) * (y1 - y0) / (x1 - x0)

        ! Calculate the absolute error in successive approximations
        abs_error = abs(y - y_prev)

        ! Display iteration information
        write (*, '(A, I2, 5X, A, F8.5, 5X, A, F8.5)') "Iteration:", i, &
               "Interpolated value:", y, "Absolute error:", abs_error

        ! Update previous approximation
        y_prev = y

        ! Check convergence
        if (abs_error < 1.0E-6) then
            write (*,*) "Convergence reached."
            exit
        endif
    end do
    write (*,*) "Maximum number of iterations reached."
end program convergence_measure
