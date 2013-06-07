	program PDE3
	implicit none

	real(8), parameter :: pi = 3.1416
	real(8) :: u(0:10, 0:100)
	real(8) ::  h, dt
	real(8) :: x
	integer :: i, t, iter

	open (10, file = 'PDE3.dat')	
	
	h = 0.1
	u = 0
	t = 0
	dt = 0.01
	do i = 0, 10
		x = i * h
		u(i, 0) = sin(pi * x)
		u(i, 1) = sin(pi * x) + dt * (1 - x) * x
	end do

	do t = 1, 99
		do i = 1, 9
			u(i, t + 1) = 2 * (1 - (dt / h) ** 2) * u(i, t) + (dt / h) ** 2 * (u(i + 1, t) + u(i - 1, t)) - u(i, t - 1)
		end do
	end do

	write(10, *) u(:, 50)


	end program PDE3

