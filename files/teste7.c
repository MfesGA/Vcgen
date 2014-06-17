
pre: n >= 0 && r == 1;
b=(r-1)*(r-1);
while (r*r < n){
inv: n >= 0 && (r-1)*(r-1) <= n;
r = r+1;}
if(r * r > n)
{r = r-1;}
else{}
pos: r*r <= n && r+1*r+1 > n;
