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
y=c(3:10,30,50)
z=c(x,y)
m=length(x)
n=length(y)
mxy=median(z)
a=sum(x>mxy)  #a=9
b=sum(y>mxy)  #b=2
p=phyper(b,n,m,a+b) #p=0.0149865614261899<0.05，所以我们拒绝H0，认为Mx>My
p=phyper(a,m,n,a+b) #当a<b时，即H1:Mx<My

#中位数之差的点估计
g = median(outer(x,y,"-"))

#中位数之差的95%区间估计
D=sort(as.vector(outer(x,y,"-")))
qwilcox(0.025,m,n)
(Dwa/2,Dmn+1-wa/2)为区间

#Wilcoxon(Mann-Whitney)秩和检验
wxy=sum(outer(y,x,"-")>0)  #wxy=21
pwilcox(21,m,n)  #p=0.004478494<0.05
    #检验表：H1:Mx>My  Wxy或Wy                   P(K<=k)
            H1:Mx<My  Wyx或Wx                   P(K<=k)
            H1:Mx!=My min(Wxy,Wyx)或min(Wx,Wy)  2P(K<=k)


#Wilcoxon两配对检验
     H1:mx!=my
x = c(15,21,18,13,35,10,17,23,14,25)
y = c(17,18,25,16,40,8,21,31,22,25)
d=x-y
rd=rank(abs(d))
w1=sum(rd*(x-y>0))
w2=sum(rd*(x-y<0))
2*psignrank(min(sum(x<y),sum(x>y)),10)     [1] 0.005859375  拒绝原假设，有显著差异

wilcox.test(x,y,paired=T)
     #结果如下：
	Wilcoxon signed rank test with continuity correction
data:  x and y
V = 5, p-value = 0.04346
alternative hypothesis: true location shift is not equal to 0   拒绝原假设

#T检验
t.test(x,y)
