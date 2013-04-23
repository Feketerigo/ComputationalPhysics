	program RungeKutta
	Implicit  none 
	real(8) :: x0, y0, x1, y1, h, kx1, kx2, kx3, kx4, ky1, ky2, ky3, ky4
	integer :: nstep, ix

	open(unit = 10, file = "data.txt")	
	write (*, *) "Enter step size :"
	read (*, *) h

	if (h <= 0) then
		stop
	end if

	nstep = 200. / h

	x0=1.
	y0=1.

	do ix = 1, nstep - 1

		kx1 = (-0.73) * y0
		ky1 = x0 * 0.34
		kx2 = (-0.73) * (y0 + ky1 * h / 2.)
		ky2 = (x0 + h / 2. * kx1) * 0.34
		kx3 = (-0.73) * (y0 + ky2 * h / 2.)
		ky3 = (x0 + h / 2. * kx2) * 0.34
		kx4 = (-0.73) * (y0 + ky3 * h)
		ky4 = (x0 + h * kx3) * 0.34
		
		x0 = x0 + h * (kx1 + 2. * kx2 + 2. * kx3 + kx4) / 6.0
		y0 = y0 + h * (ky1 + 2. * ky2 + 2. * ky3 + ky4) / 6.0
		
		write (10, *)ix, x0, y0
		write (*, *)ix, x0, y0

	end do
	

	end program RungeKutta
