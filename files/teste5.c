pre: (x >= (0-100)) && (x <= 100);
     
if (x < 0)  {
	x = x + 100 ;
}else{
    x=x;
}

y = 2 * x;

pos : (y >= 0) && (y <= 300);
