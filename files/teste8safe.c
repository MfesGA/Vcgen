
pre:  b==bb  && k==kk && z==zz && j==jj && 0 <= z && 0<=j && z<=j &&  k<=jj+1;
 
k=z;
while(k<=j){
inv: k<=z+z-j; 
k=k+1;
b=b+1;
}
kk=jj;
while(kk>=zz){
inv: k<=jj-zz ;
kk=kk-1;
bb=bb+1;
}
kk=jj+1;

pos: k==kk && b==bb ;
