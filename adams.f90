	program adams
	implicit none
	
	integer :: nstep, ix
	real(8) :: fx1, fx2, fx3, fx4, fy1, fy2, fy3, fy4, y1, y2, y3, y4, x1, x2, x3, x4, h, x, y
		
	write (*, *) "Enter step size :"
	read (*, *) h
	if (h <= 0) then
		stop
	end if
	nstep = 3. / h
	
	x1 = 1.
	y1 = 1.
	y1 = y1 + h * 0.34 * x1
	x1 = x1 + h * (-0.73) * y1
	y2 = y1 + h * 0.34 * x1
	x2 = x1 + h * (-0.73) * y1
	y3 = y2 + h * 0.34 * x2
	x3 = x2 + h * (-0.73) * y2
	y4 = y3 + h * 0.34 * x3
	x4 = x3 + h * (-0.73) * y3

	do ix = 4, nstep - 1
		fx1 = (-0.73) * y1
		fx2 = (-0.73) * y2
		fx3 = (-0.73) * y3
		fx4 = (-0.73) * y4
		fy1 = 0.34 * x1
		fy2 = 0.34 * x2
		fy3 = 0.34 * x3
		fy4 = 0.34 * x4
		x = x4 + h / 24 * (55 * fx4 -59 * fx3 + 37 * fx2 - 9 * fx1)
		y = y4 + h / 24 * (55 * fy4 -59 * fy3 + 37 * fy2 - 9 * fy1)
		x = x4 + h / 24 * (9 * (-0.73) * y + 19 * fx4 - 5 * fx3 + fx2)
		y = y4 + h / 24 * (9 * 0.34 * x + 19 * fy4 - 5 * fy3 + fy2)
		write(*, *) ix, x, y
		x1 = x2
		y1 = y2
		x2 = x3
		y2 = y3
		x3 = x4
		y3 = y4
		x4 = x
		y4 = y
	end do

	end program adams