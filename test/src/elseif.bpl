void main(void) {
    int x;
    int y;
    int z;

    x = 0;
    y = 1;
    z = 1;

    if(x == 1) {
        write("x == 1");
    } else if(y == 1) {
        write("y == 1");
    } else if(z == 1) {
        write("x != 1, y != 1, and z == 1");
    } else {
        write("None of x, y, or z equal 1.");
    }

    writeln();
}
