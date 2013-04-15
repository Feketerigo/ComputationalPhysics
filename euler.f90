	program euler
	implicit none

	integer :: nstep, ix
	real(8) :: h, y, x, diff

	write (*, *) "Enter step size :"
	read (*, *) h

	if (h <= 0) then
		stop
	end if

	nstep = 3. / h
	x = 1.
	y = 1.
	do ix = 1, nstep - 1
		y = y + h * 0.34 * x
		x = x + h * (-0.73) * y
		write (*, *)ix, x, y
	end do

	end program euler