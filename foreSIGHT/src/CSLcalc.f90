SUBROUTINE CSLcalc(x,y,n)

double precision,intent(in) :: x(n)
integer, intent(in) :: n
double precision,intent(inout) :: y
integer :: i, half, sum, sum2, len
double precision :: m
m=n*1

half=FLOOR((m/2)-1)
len=n-5
sum=0
sum2=0

do i=6,half
  if((x(i-5)<15).AND.(x(i-4)<17).AND.(x(i-3)<17).AND.(x(i-2)<17).AND.(x(i-1)<17).AND.(x(i)<17)) then
  sum=half-i
  exit
  end if
end do

do i=half+1,len
  if((x(i)>17).AND.(x(i+1)>17).AND.(x(i+2)>17).AND.(x(i+3)>17).AND.(x(i+4)>17).AND.(x(i+5)>17)) then
  sum2=i-(half+1)
  exit
  end if
end do


y=sum+sum2

END SUBROUTINE CSLcalc
