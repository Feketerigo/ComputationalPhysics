	program DIFFERENTIAL
	implicit none
		
		real :: h
		real :: x
		real :: f
		real :: df
		real :: exact
		real :: error

		write (*, *) "Enter x:"
		read (*, *) x
		write (*, *) "Enter h:"
		read (*, *) h
		
		exact = cos (x)
		df = (sin (x+h)-sin (x-h)) / (2*h)
		error = abs (df - exact)
		
		write (*, *) df, error

	end program