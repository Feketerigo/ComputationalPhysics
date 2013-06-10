	program eigenvalue
	implicit none
		real(8) :: h, x, f, fx, diff, fix
		real(8),parameter :: toldiff = 0.000001
		integer :: i

		x = 0
		f = 0
		fix = -1

		do i = 1, 3
			h = 0.1
			do while (h > toldiff)
				x = x - h
				fx = (-2-x)**3-2*(-2-x)
				if (fix * fx < 0) then
					x = x + h
					h = h / 2.0
				end if
			end do
			write(*, *) x
			x = x - 1
			fix = fx
		end do

	 end program