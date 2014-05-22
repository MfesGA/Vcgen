pre: x == 8 && y == 16;

while ( x > 0){
	inv: y == 2 * x && x >= 0;
	x = x - 1;
	y = y - 2;
}

pos: y == 0;
