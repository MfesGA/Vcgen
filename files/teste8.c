
pre:  b==bb  && k==kk && z==zz && j==jj && 0 <= z && 0<=j && z<=j &&  k<=jj+1;
 
k=z;
while(k<=j){
inv: b==bb+(z-k) && z<=k && k <=j+1; 
k=k+1;
b=b+1;
}


pos: b==bb+(z-j+1);
