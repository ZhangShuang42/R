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
