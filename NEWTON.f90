	program NEWTON
	implicit none

	integer :: n
	real(8) :: x0 ,x1, x2, f0, fd0, f1, fd1, diff, dx
	real(8), parameter :: toldiff = 0.0000001

	n = 0
	x0 = 1.0

	diff = 1.694600802400142 - x0
	write(*, *) n, x0, diff

	do while(.true.)
		n = n + 1
		f0 = exp(x0) * log(x0) - x0 * x0
		fd0 = exp(x0) / x0 + exp(x0) * log(x0) -2 * x0
		x1 = x0 - f0 / fd0
		diff = 1.694600802400142 - x1
		write(*, *) n, x1, diff
		dx = x1 - x0
		if (abs(dx)  >= toldiff) then
			x0 = x1
		else
			stop
		end if
	end do
			
	end program NEWTON