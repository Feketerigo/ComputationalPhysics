	program SIMPSON
	implicit none
	
	real :: a
	real :: b
	real :: h
	real :: si
	real :: f1
	real :: f2
	real :: s
	real :: answer
	integer :: i
	integer :: n

	write (*, *) "Enter a and b:"
	read (*, *) a, b
	write (*, *) "Enter n (EVEN):"
	read (*, *) n
	
	if (mod (n, 2) ==1) then
		write (*, *) "Error"
		stop
	end if

	h = (b - a) / n
	s = 0

	do i = 2, n - 2 , 2		
		f1 = 4 * exp (a + (i-1) * h)
		f2 = 2 * exp (a + i * h)
		si = f1 + f2 
		s = s + si
	end do

	answer = (s + exp (a) + exp (b) + 4 * exp (b-h)) * h / 3

	write (*, *) answer

	

	end program SIMPSON

