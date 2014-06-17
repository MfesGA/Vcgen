pre: exists: h, 0 <= h && h <= (n-1) && v[h] == x;

k = 0;

while (v[k] != x){
    inv: exists: y,  k<=y && y <= n-1 && v[y] == x;
    k=k+1;
}

pos: v[k] == x;

