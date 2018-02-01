    SUBROUTINE R10calc(x,y,n)

double precision,intent(in) :: x(n)
integer, intent(in) :: n
double precision,intent(inout) :: y
integer :: i,sum

sum=0

do i=1,n
  if(x(i)>10) then
  sum=sum+1
  end if
end do
y=sum

END SUBROUTINE R10calc