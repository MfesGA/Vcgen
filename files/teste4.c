pre: size >=1;


max = 0;
n = 1;


while(n < size ){
    inv: 1 <= n  && n <= size && 0 <= max && max < n && (forall: a, (0 <= a && a < n) :->  (u[a] <= u[max]));
    if(u[n]>u[max]){
        max = n;
    }else{}

    n=n+1;
}

pos:  0 <= max  && max < size && (forall: a, 0 <= a &&  a < size  :-> u[a] <= u[max]);