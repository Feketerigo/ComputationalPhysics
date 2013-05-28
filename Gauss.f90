program Gauss
implicit none

	real(8) :: m(4, 3), temp(4)
	real(8) :: maxvalue
	integer :: i, j, k
	real(8) :: p1, p2

	m = reshape((/2,1,2,5,5,-1,1,8,1,-3,-4,-4/),(/4, 3/))

	i = 1
!	do i = 1, 4
		do j = 1, 3
			maxvalue = 0.
			if (maxvalue < m(i, j)) then
				maxvalue = m(i, j)
				do k = 1, 4
					temp(k) = m(k, j)
					m(k, j) = m(1, j)
					m(1, j) = temp(k)
				end do
			end if
		end do
		p1 = m(i, 2) / m(i, 1)
		p2 = m(i, 3) / m(i, 1)
		m(:, 2) = m(:, 2) - m(:, 1) * p1
		m(:, 3) = m(:, 3) - m(:, 1) * p2
!	end do
	write(*, *) m

end program Gauss