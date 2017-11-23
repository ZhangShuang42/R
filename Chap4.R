#Kruskal-Wallis秩和检验

x = read.table("D:/data/wtloss.txt")
x
    V1 V2
1  3.7  1
2  3.7  1
3  3.0  1
4  3.9  1
5  2.7  1
6  7.3  2
7  5.2  2
8  5.3  2
9  5.7  2
10 6.5  2
11 9.0  3
12 4.9  3
13 7.1  3
14 8.7  3
x1=x[,1]
x[,2]==2
[1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
x[x[,2]==2,1]
[1] 7.3 5.2 5.3 5.7 6.5
x1=x[x[,2]==1,1]
x2=x[x[,2]==2,1]
x3=x[x[,2]==3,1]
kruskal.test(list(x1,x2,x3))
  #结果：
	Kruskal-Wallis rank sum test
data:  list(x1, x2, x3)
Kruskal-Wallis chi-squared = 9.4322, df = 2, p-value = 0.00895  #拒绝原假设


#Jonckheere-Terpstra检验

u12=sum(outer(x1,x2,'-')<0)
u13=sum(outer(x1,x3,'-')<0)
u23=sum(outer(x2,x3,'-')<0)
J=u12+u13+u23
N=length(c(x1,x2,x3))
z=(J-(N^2-25-25-16)/4)/((N^2*(2*N+3)-25*(2*5+3)-25*13-16*11)/72)^0.5   #z=3.10336223197634
pnorm(z,low=F)
[1] 0.0009566765    #拒绝原假设
