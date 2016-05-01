int test(void) {
    if(5 < (3 + 4)) {
        return 7 - 5 * 4 + 3;
    }
    write("This should not appear in the optimized intermediate representation.");
    writeln();
    write("Also, the lack of a return statement after the if-statement above should not cause an error.");
    writeln();
}

void main(void) {
    int x;
    int y;
    x = -7;
    x = 5 < (3 + 4);
    y = 4 - x + 5;
}
