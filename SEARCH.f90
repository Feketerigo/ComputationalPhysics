	program SEARCH
	implicit none
	
	integer :: n
	real(8) :: h, x, f, fx, diff
	real(8),parameter :: toldiff = 0.000001

	n = 0
	x = 1.0
	h = 0.5

	diff = 1.694600802400142 - x
	write (*, *) n, x, diff

	do while (h > toldiff)
		n = n + 1
		x = x + h
		diff = 1.694600802400142 - x
		write (*, *) n, x, diff
		fx = exp(x) * log(x) - x * x
		if (f - fx < 0) then
			x = x - h
			h = h / 2.0
		end if
	end do

	end program SEARCH

