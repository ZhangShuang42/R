##Wilcoxon符号秩检验
#习题2.6.17
x <- c(16.7,17.7,14.1,11.4,13.4,10.5,13.6,11.6,12.0,12.6,11.7,13.7)
wilcox.test(x,mu=12,alternative = "greater",
exact=F,correct=F,confi.int=T)
#结果输出：

	Wilcoxon signed rank test

data:  x
V = 53.5, p-value = 0.03411
alternative hypothesis: true location is greater than 12

#Brown-Mood中位数检验
x=c(11:20,40,60)
length(x)
y=c(3:10,30,50)
length(y)
z=c(x,y)
length(z)
m=length(x)
n=length(y)
mxy=median(z)
a=sum(x>mxy)  #a=9
b=sum(y>mxy)  #b=2
p=phyper(b,n,m,a+b) #p=0.0149865614261899<0.05，所以我们拒绝H0，认为Mx>My
p=phyper(a,m,n,a+b) #当a<b时，即H1:Mx<My
