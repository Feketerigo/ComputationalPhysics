program Numerov             !用Numerov算法求解电荷分布产生的静电势
implicit none 
	integer:: l,j
	real(8):: phi0, phi1, phi2, r0, r1, r2, h, s0, s1, s2, con, exact, diff
	open(10,file="potential.txt")

	h=0.1
	con=h**2/12.0
	r0=0
	r1=r0+h
	phi0=0
	phi1=1-0.5*(r1+2)*exp(-r1)

	do while(.true.)
		s0=-0.5*r0*exp(-r0)
		s1=-0.5*r1*exp(-r1)
		r2=r1+h
		s2=-0.5*r2*exp(-r2)

		phi2=2*phi1-phi0+con*(s2+10*s1+s0)
		exact=1-0.5*(r2+2)*exp(-r2)
		diff=exact-phi2
		
		if(r2<20.01)then
			write(10,*)r2,exact,diff
			write(*,*)r2,exact,diff
			r0=r1
			r1=r2
			phi0=phi1
			phi1=phi2
		else
			stop
		end if
	end do

end
