#Spearman & Kendall 相关检验
x=c(45,52,54,63,62,68,75,76,92,88)
y=c(100,111,120,133,140,152,160,171,180,195)
cor.test(x,y,method = "spearman")
  #结果：
	Spearman's rank correlation rho
data:  x and y
S = 4, p-value < 2.2e-16
alternative hypothesis: true rho is not equal to 0
sample estimates:
      rho 
0.9757576 

cor.test(x,y,method = "kendall")
  #结果：
	Kendall's rank correlation tau
data:  x and y
T = 43, p-value = 2.976e-05
alternative hypothesis: true tau is not equal to 0
sample estimates:
      tau 
0.9111111 


#Theil回归
d=read.table("d:/data/CPIGINI.txt",header=T)
x=d[,1]
y=d[,2]
n=nrow(d)
s=NULL
for(i in 1:(n-1))for(j in (i+1):n)s=c(s,(y[j]-y[i])/(x[j]-x[i]))
b=median(s)
a=median(y-b*x)
e=y-a-b*x
coef=c(a,b)
  #结果：a=43.65;b=-1.667
