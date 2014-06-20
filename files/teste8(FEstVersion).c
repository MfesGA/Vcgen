
pre: j==jj && z==zz && b==bb && k==kk && 0<= z && 0<=j && z<=j;
 
k=z;
while(k<=j){
inv: bb==b+(kk-z) && i<=kk &&kk <=j+1; 
k=k+1;
b=b+1;
}

kk=jj;
while(kk>=zz){
inv: bb==b+(j-kk) && z-1<=kk && kk <= j;
kk=kk-1;
bb=bb+1;
}
kk=jj+1;

pos: kk==k && bb==b ;
