	program PDE2
	implicit none

	integer :: i, ite
	integer, parameter :: N = 26
	real(8) :: h, dt, dth, p1, p2
	real(8) :: phi(N), A(N, 3), R(N)
	real(8) :: t, x

	open(10, file = "PDE2.dat")
	h = 1.0 / (n - 1)
	dt = 0.01
	dth = dt / h ** 2
	p1 = 1 / dth + 1
	p2 = 1 / dth - 1
	
	t = 0
	do i = 1, N
		x = (i - 1) * h
		phi(i) = exp(-20 * (x - 0.5)**2) - exp(-20 * (x - 1.5)**2) - exp(-20 * (x + 0.5)**2)
	end do
	
	call coef(N, h, p1, A)

	do ite = 1, 100
		t = ite * dt
		R(1) = -1 * h * phi(1)
		R(N) = -1 * h * phi(N)
		do i = 2, N - 1
			R(i) = phi(i - 1) + 2 * p2 * phi(i) + phi(i + 1)
		end do
		call solve(N, A, R)
		do i = 1, N
			phi(i) = R(i)
		end do
		if (ite == 12) then
			t = ite * dt
			do i = 1, N
				write(10, *) phi(i)
			end do
		end if
	end do 

	end program PDE2

	
	
	subroutine coef(N, h, p1, A)
	integer :: N
	real(8) :: A(N, 3), h, p1
	A(1, 2) = -1
	A(1, 3) = 0
	A(N, 2) = h
	A(N, 1) = 0
	do i = 2, N - 1
		A(i, 1) = 1
		A(i, 2) = 2 * p1
		A(i, 3) = 1
	end do
	do i = 2, N
		A(i, 2) =A(i, 2) - A(i, 1) * A(i - 1, 3) / A(i - 1, 2)
	end do
	return
	end subroutine



	subroutine solve(N, A, R)
	integer :: N
	real(8) :: A(N ,3), R(N)
	do i = 2, N
		R(i) = R(i) - A(i, 1) * R(i - 1) / A(i - 1, 2)
	end do
	R(N) = R(N) / A(N, 2)
	do ii = 1, N - 1 
		i = N - ii
		R(i) = (R(i) - A(i ,3) * R(i + 1)) / A(i, 2)
	end do
	return
	end subroutine