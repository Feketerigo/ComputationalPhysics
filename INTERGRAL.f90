	program INTERGRAL

	implicit none

	real :: a
	real :: b
	real :: h
	real :: si
	real :: f1
	real :: f2
	real :: s
	integer :: i
	integer :: n

	write (*, *) "Enter a and b:"
	read (*, *) a, b
	write (*, *) "Enter n:"
	read (*, *) n
	h = (b-a) / n

	do i = 1, n
		f1 = exp (a + (i-1) * h)
		f2 = exp (a + i * h)
		si = (f1 + f2) * h / 2
		s = s + si
	end do

	write (*, *) s


	end program INTERGRAL

