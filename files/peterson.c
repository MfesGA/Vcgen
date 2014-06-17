pre:(max >=0 && (forall: a, a>=0 && a <= max && u[a] >=0));

m=0;
max=10;
find = 2;
u[9] = 2;
place = 0;
while( m <= max ){
    inv: (exists: a, (a >=0  && a <= max) :-> u[a] == find);

    if(u[m] == find){
        place = m;
    }else{}

    m = m+1;
}

pos: place == 9;