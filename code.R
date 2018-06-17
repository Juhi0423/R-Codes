setwd("C:/Users/Administrator/Desktop/p1")
x=153
digit1<-as.numeric(substr(x,1,1))
digit1
digit1.cube<-digit1^3
digit1.cube
digit2<-as.numeric(substr(x,2,2))
digit2
digit2.cube<-digit2^3
digit2.cube

digit3<-as.numeric(substr(x,3,3))
digit3
digit3.cube<-digit3^3
digit3.cube


if(x== sum(digit1.cube,digit2.cube,digit3.cube)){
  print("Armstrong Number")
}else{
  print("Not an Armstrong Number")
}



is.arm<-function(x)
{
  result<-0
for(i in 1:nchar(x))
 {
  digit<-as.numeric(substr(x,i,i))
  digit.cube<-digit^3

result=result+digit.cube
 }
  if(result == x)
  {
    print("arm")
  }else{
    print("not arm")
  }
}

is.arm(123)


#######strong number
fact<-function(x)
{
  if(x==1|x==0)
    return(1)
  else return(x*fact(x-1))
}
fact(4)

y=145
substr(145,1,1)
r<-0
for(i in 1:nchar(y))
{
  d<-as.numeric(substr(y,i,i))
  r=fact(d)+r
  print(r)
}
 if(r==y)
 {
   print("Strong Number")
 }else
 {
   print("Not Strong")
 }




######perfect number
perfect<-function(z)
{
  r<-0
  for(i in 1:nchar(z))
  {
    d<-as.numeric(substr(z,i,i))
    d.sqr<-digit^2
    r=r+d.sqr
  }
  if(r == z)
  {
    print("Perfect Number")
  }else{
    print("Not Perfect Number")
  }
}






