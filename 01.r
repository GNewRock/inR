VM=100;
N=5;
M=5;
v1<-runif(VM)
v2<-v1+rnorm(VM, 0, 1)
plot(v1,v2)
n<-matrix(0, N, M)
for(i in 1:VM)
{
	for(j in 1:N)
	{
		for(k in 1:M)
		{
			if(v1[i]<=(max(v1)-min(v1))*j/N+min(v1) && v1[i]>=(max(v1)-min(v1))*(j-1)/N+min(v1) && v2[i]<=(max(v2)-min(v2))*k/M+min(v2) && v2[i]>=(max(v2)-min(v2))*(k-1)/M+min(v2))
			{
				n[j,k]=n[j,k]+1
			}
		}
	}
}
n_<-matrix(0, N, M)
for(k in 1:N){
	for(l in 1:M){
		n_[k,l]=mean(n[k,])*mean(n[,l])/mean(n)
	}
}
mean((n-n_)^2/n_)*N*M


