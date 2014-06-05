pre: n > 100;

k = 0;
r = 0;
s = 1;

while( k != n){
    inv: r == k * k;
    r = r + s;
    s = s + 2;
    k = k + 1;
}

pos: r == n * n;
