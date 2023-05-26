#intro 
A=matrix(c(1,2,2,-2),2,2) #mat 
ee=eigen(A,symmetric=TRUE) #egin value and vector
B=matrix(c(3,5,4,6),2,2)  
A*B      ## elementwise product
A%*%B   ## matrix product da elly bn7tago
t(A)    # trans   
solve(A)  #inv
colMeans(A) # mean of colum 
cor(A)  # corelation 
diag=diag(ee$values , nrow = 2) #make diag mat

#lec 1 
A=matrix(c(1,2,2,-2),2,2)  
ee=eigen(A,symmetric=TRUE)
ee$values   
ee$vectors


#lec 2 
#Q1  A=QΩQ^t
A=matrix(c(1,2,2,-2),2,2)  
ee=eigen(A,symmetric=TRUE)
diag=diag(ee$values , nrow = 2) #Ω
e=ee$vectors #Q
e 
diag

z=e%*%diag%*%t(e)
z
A


#part2 , A^-1 = QΩ^-1 Q^t
z2= e%*%solve(diag)%*%t(e)
z2
solve(z)

#Q2 lec 2 find mean , var, R
x=matrix(c(41,57,49,46,37,3,5,5,4,3),ncol = 2) #2features
x
n=5              # number of obvs 
tx=t(x)
tx               #trans x mat 
m=colMeans(x)
m=t(m)
m                 #mean mat
tm=t(m)
tm               # trans mean mat

s1=(1/(n-1))*((tx %*% x)-(n*tm%*%m))
s1

s=cov(x) # easy way to find var 
s


v=diag(diag(s)) # second diag (59,1) first to make it in mat 
v
vinv=solve(v^(1/2))
vinv



#corr
corr=vinv%*%s%*%vinv
corr
cor(x) # easy way to find corr 

#lec3
mu=matrix(c(2,-1,3))
segma=matrix(c(2,-1,0,-1,9,3,0,3,4),ncol=3)
#y=2x1-3x2+x3
y=matrix(c(2,-3,1))
#E(y)
E_y=t(y)%*%mu
E_y
#var(y)
var_y=t(y)%*%segma%*%y
var_y

#lec4
#find y1,y2 distribution
#y1=x1+x2 , y2=2x2-x3
mu=matrix(c(1,1,2))
segma=matrix(c(1,1,1,1,3,2,1,2,2),ncol=3)
#y=transpose([y1,y2])
y=matrix(c(1,0,1,2,0,-1),ncol=3)
#E(y)
E_y=y%*%mu
E_y
#var(y)
var_y=y%*%segma%*%t(y)
var_y
#y1~N(2,6)   &&  y2~N(0,6)






#lec 5
mu=matrix(c(5,5,10,10))
mu
segma=matrix(c(10,4,4,2,4,20,10,10,4,10,20,8,2,10,8,20),ncol=4)
segma

#condition [x1,x4] given x2=x3=5
#rearange mu && segma

x2=x3=5
new_mu=t(t(mu[c(1,4,2,3)]))
new_mu
new_segma=segma[c(1,4,2,3),c(1,4,2,3)]
new_segma


mu12=t(t(new_mu[c(1,2)]))+new_segma[c(1,2),c(3,4)]%*%solve(new_segma[c(3,4),c(3,4)])%*%(5-t(t(new_mu[c(3,4)])))
segma12=new_segma[c(1,2),c(1,2)]-new_segma[c(1,2),c(3,4)]%*%solve(new_segma[c(3,4),c(3,4)])%*%new_segma[c(3,4),c(1,2)]
#f((x1,x4)| (x2,x3)) ~ N(mu12 ,segma12)
mu12
segma12




#lec 6
x=matrix(c(6,10,8,9,6,3),ncol=2)
mu=matrix(c(9,5),ncol=1)
#perform Hotelling Test in95%
x_par=colMeans(x) # mean of columns
x_par
nrow(x) # size of row 
#covarince matrix
s=(1/(nrow(x)-1))*((t(x)%*%x)-(nrow(x)*x_par%*%t(x_par)))
s
T0=nrow(x)*t(x_par-mu)%*%solve(s)%*%(x_par-mu)
T0
tc=(((nrow(x)-1)*ncol(x))/(nrow(x)-ncol(x)))*qf(p=.05, df1=ncol(x), df2=nrow(x)-ncol(x),lower.tail=FALSE)
tc
# T0 =0.77777 <  TC=798    Then don't reject H0




#lec7
x=matrix(c(3.17,3.45,3.73,1.82,4.39,2.91,3.54,4.09,2.85,2.05,4.35,2.35,5.04,3.88,3.64,4.63,2.88,3.98,3.74,4.36),ncol=2)
x
mu=matrix(c(3,5),ncol=1)
mu
#perform Hotelling Test in95%
x_par=colMeans(x) # mean of columns
x_par
#covarince matrix
s=(1/(nrow(x)-1))*((t(x)%*%x)-(nrow(x)*x_par%*%t(x_par)))
s
solve(s)
T0=nrow(x)*t(x_par-mu)%*%solve(s)%*%(x_par-mu)
T0
tc=(((nrow(x)-1)*ncol(x))/(nrow(x)-ncol(x)))*qf(p=.05, df1=ncol(x), df2=nrow(x)-ncol(x),lower.tail=FALSE)
tc
# T0 =19.3339 >  TC=10.03268   Then  reject H0




#in univariate
ee=eigen(s,symmetric=TRUE) #eigen values and vectors
ee
a=sqrt(ee$values[1]*(tc/nrow(x)))
a
b=sqrt(ee$values[2]*(tc/nrow(x)))
b
#which feature cause of reject H0
upper_mu1=x_par[1]+sqrt(tc*(s[1,1]/nrow(x)))
lower_mu1=x_par[1]-sqrt(tc*(s[1,1]/nrow(x)))
upper_mu2=x_par[2]+sqrt(tc*(s[2,2]/nrow(x)))
lower_mu2=x_par[2]-sqrt(tc*(s[2,2]/nrow(x)))
#[3,5] 3 in range of mu1   , 5 not on range of mu2
#B is reason of reject H0


#lec 8
#using Bonferroni c.i
B_upper_mu1=x_par[1]+qt(p=0.0125, df=9,lower.tail=FALSE)*sqrt((s[1,1]/nrow(x)))
B_lower_mu1=x_par[1]-qt(p=0.0125, df=9,lower.tail=FALSE)*sqrt((s[1,1]/nrow(x)))
B_upper_mu2=x_par[2]+qt(p=0.0125, df=9,lower.tail=FALSE)*sqrt((s[2,2]/nrow(x)))
B_lower_mu2=x_par[2]-qt(p=0.0125, df=9,lower.tail=FALSE)*sqrt((s[2,2]/nrow(x)))

#three drugs example
DEFF_MAT=matrix(c(-0.05,-0.15,-0.53,-0.54,-0.4,0.17,-0.31,-0.28,-0.04,0.09,0.11,0.04,0.11,0.11,0,0.03,0.09,0.06,0.16,0.1,-0.11,0.02,0.01,-0.04,0,0.02,0.05,0.03,0.02,-0.01),ncol=3)
DEFF_MAT
x_par=colMeans(DEFF_MAT) # mean of columns
s=(1/(nrow(DEFF_MAT)-1))*((t(DEFF_MAT)%*%DEFF_MAT)-(nrow(DEFF_MAT)*x_par%*%t(x_par)))
s
solve(s)


T0=nrow((DEFF_MAT))*t(x_par)%*%solve(s)%*%(x_par)
T0

tc=(((nrow(DEFF_MAT)-1)*ncol(DEFF_MAT))/(nrow(DEFF_MAT)-ncol(DEFF_MAT)))*qf(p=.05, df1=ncol(DEFF_MAT), df2=nrow(DEFF_MAT)-ncol(DEFF_MAT),lower.tail=FALSE)
tc

upper_mu1=x_par[1]+sqrt(tc*(s[1,1]/nrow(DEFF_MAT)))
lower_mu1=x_par[1]-sqrt(tc*(s[1,1]/nrow(DEFF_MAT)))
upper_mu2=x_par[2]+sqrt(tc*(s[2,2]/nrow(DEFF_MAT)))
lower_mu2=x_par[2]-sqrt(tc*(s[2,2]/nrow(DEFF_MAT)))
upper_mu3=x_par[3]+sqrt(tc*(s[3,3]/nrow(DEFF_MAT)))
lower_mu3=x_par[3]-sqrt(tc*(s[3,3]/nrow(DEFF_MAT)))

lower_mu1
upper_mu1

#c.i of d2 does not include 0 thus H0 was rejected due to the second component (X2)


#lec 9
x_par=matrix(c(46.1,57.3,50.3),ncol = 1)
s=matrix(c(101.3,63,71,63,80.2,55.6,71,55.6,97.4),ncol=3)

x_par
s

#Test differance of mean in dices at alpha=0.05
A=matrix(c(1,1,-1,0,0,-1),ncol = 3)
A

T0=40*t(A%*%x_par)%*%solve(A%*%s%*%t(A))%*%(A%*%x_par)
T0

tc=(((40-1)*(3-1))/(40-3+1))*qf(p=.05, df1=2, df2=38,lower.tail=FALSE)
tc

#T0=90.5574   >  tc =6.66041 reject H0


#mu1-mu2
upper_mu1_mu2=x_par[1]-x_par[2]+sqrt((tc/40)*(s[1,1]+s[2,2]-2*s[1,2]))
lower_mu1_mu2=x_par[1]-x_par[2]-sqrt((tc/40)*(s[1,1]+s[2,2]-2*s[1,2]))

#mu1-mu3
upper_mu1_mu2=x_par[1]-x_par[3]+sqrt((tc/40)*(s[1,1]+s[3,3]-2*s[1,3]))
lower_mu1_mu2=x_par[1]-x_par[3]-sqrt((tc/40)*(s[1,1]+s[3,3]-2*s[1,3]))

#mu2-mu3
upper_mu1_mu2=x_par[2]-x_par[3]+sqrt((tc/40)*(s[2,2]+s[3,3]-2*s[2,3]))
lower_mu1_mu2=x_par[2]-x_par[3]-sqrt((tc/40)*(s[2,2]+s[3,3]-2*s[2,3]))

#mu2>mu3>mu1