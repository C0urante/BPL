void main(void) {
	int x;
	int *p;
	x = 37;
	p = &x;
	x = *p+1;
	write("x =");
	write(x);
	writeln();
	write("*p =");
	write(*p);
	writeln();
}
