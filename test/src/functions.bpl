string stringmimic(string result) {
    return result;
}

int intmimic(int result) {
    return result;
}

int five(void) {
    return 5;
}

int add(int x, int y) {
    return x + y;
}

int sub(int x, int y) {
    return x - y;
}

string hello(void) {
    return "Hello!";
}

string goodbye(void) {
    return "Goodbye!";
}

void main(void) {
    int simple;
    int moderate;
    int complex;

    write(stringmimic(stringmimic(hello())));
    writeln();

    simple = five();
    write("5 =");
    write(simple);
    writeln();

    moderate = add(5, 10);
    write("5 + 10 =");
    write(moderate);
    writeln();

    complex = sub(add(five(), five()), five());
    write("(5 + 5) - 5 =");
    write(complex);
    writeln();

    write(goodbye());
    writeln();
}
