void main(void) {
    int v;
    int *ptr;
    int a[10];

    v = 5;
    ptr = &a[4];
    *ptr = v;

    write("v = ");
    write(v);
    writeln();

    write("a[4] = ");
    write(a[4]);
    writeln();

    write("*ptr = ");
    write(*ptr);
    writeln();
}
