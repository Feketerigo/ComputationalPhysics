	program PDE1
	implicit none
	
	real(8), parameter :: pi = 3.1416
	real(8) :: u(0:40, 0:30)
	real(8) :: omega, h
	real(8) :: x, y
	integer :: i, j, iter

	open (10, file = 'PDE1.dat')	
	omega = 1.24
	h = 0.1
	u = 0
	
	do i = 0, 30
		y = i * h
		u(0, i) = y * (y - 3)
	end do
	do i = 0, 40
		x = i * h
		u(i, 0) = sin(pi * x / 4)
	end do

	do iter	= 1, 500
		do i = 1, 39
			do j = 1, 29
				u(i, j) = omega / 4 * (u(i, j-1) + u(i-1, j) + u(i, j+1) + u(i+1, j)) + (1 - omega) * u(i, j)
			end do
		end do
	end do

	write(10, "(F12.9)", advance = 'NO') u
	
	end program PDE1