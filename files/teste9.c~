pre: b==bba && b==bbb && k==ksb && ksa==k && 0<= i && 0<= j && i<=j;
ksa=i;
while(ksa<=j){
inv:  bba == b + ksa-i && i <= ksa && ksa <= j+1;
  bba=bba + 1;
  ksa=ksa + 1;
}
  

ksb=j;
while (ksb>=i) {
inv: bbb == b + j-ksb && i-1 <= ksb && ksb <= j;
  bbb=bbb+1;
  ksb=ksb-1;
}

ksb = j+1;

pos: bba==bbb && ksa==ksb;
