SUBROUTINE CDDcalc(x,y,n)

double precision,intent(in) :: x(n)
integer, intent(in) :: n
double precision,intent(inout) :: y
integer :: i, sum, max

sum=1
max=1

do i=2,n
if(x(i-1)==0) then
  if(x(i)==0) then
    sum=sum+1
  else
    sum=1
  end if

end if
if(sum>=max) then
  max=sum
end if
end do
y=max

END SUBROUTINE CDDcalc
