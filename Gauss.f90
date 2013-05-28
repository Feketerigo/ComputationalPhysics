program Gauss
implicit none

	real(8) :: m(4, 3), temp(4), p(3)
	real(8) :: maxvalue
	integer :: i, j, k
	real(8) :: x, y, z

	m = reshape((/2,1,2,5,5,-1,1,8,1,-3,-4,-4/),(/4, 3/))

	do i = 1, 2
		maxvalue = m(i, 1)
		do j = i, 3
			if (maxvalue < m(i, j)) then
				maxvalue = m(i, j)
			end if
		end do
		do k = i, 3
			if (m(i, k) == maxvalue) then
				temp(:) = m(:, i)
				m(:, i) = m(:, k)
				m(:, k) = temp(:)
			end if
		end do
		do k = i + 1 ,3
			p = 0
			p(k) = m(i, k) / m(i, i)
			m(:, k) = m(:, k) - m(:, i) * p(k)
		end do
	end do

	write(*, *) m

	z = m(4, 3) / m(3, 3)
	y = (m(4, 2) - m(3, 2) * z) / m(2, 2)
	x = (m(4, 1) - m(3, 1) * z - m(2, 1) * y) / m(1, 1)

	write(*, *)x, y, z

end program Gauss