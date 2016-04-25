void main(void) {
    int x;

    x = 0;
    write("x =");
    write(x);
    writeln();

    x = x + 5;
    write("x + 5 =");
    write(x);
    writeln();

    x = x * 10;
    write("(x + 5) * 10 =");
    write(x);
    writeln();

    x = x - 5;
    write("((x + 5) * 10) - 5 =");
    write(x);
    writeln();

    x = x / 8;
    write("(((x + 5) * 10) - 5) / 8 =");
    write(x);
    writeln();

    x = x % 3;
    write("((((x + 5) * 10) - 5) / 8) % 3 =");
    write(x);
    writeln();
}
