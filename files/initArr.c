

pre: n >=0  &&  o < n && o >= 0;

while(o < n){
    inv: o <= n && (forall: k, k >= 0 && k<o :-> a[k] == 0); 
    a[o] = 0;
    o=o+1;
}

pos: (forall: k, k >= 0 && k < n :-> a[k] == 0 );
