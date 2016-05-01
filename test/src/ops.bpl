int sub(int a, int b, int c, int d, int e) {
    return a - b - c - (d - e);
}

int add(int a, int b, int c, int d, int e) {
    return a + b + c + (d + e);
}

int mod(int m, int o, int d, int t, int e, int s) {
    return (m % o % d) % (t % e % s % t);
}

int div(int x, int y, int z, int x1, int x2) {
    return x / y / z / (x1 / x2);
}

int mul(int x, int y, int z, int x1, int x2) {
    return x * y * z * (x1 * x2);
}

int mulSub(int t, int e, int s, int i, int n, int g) {
    int testing1;
    int testing2;
    testing1 = t * e - s * t - i * n - g;
    testing2 = t - e * s - t * i - n * g;
    return (testing1 - testing2) * (testing2 - testing1);
}

void hello(void) {
    write("\"Hello, world!\"");
    writeln();
}

void main(void) {
    hello();

    write("69 - 32 - (-7) - (45 - 17) =");
    write(sub(69, 32, -7, 45, 17));
    writeln();

    write("(-100) + 50 + (-25) + (12 + (-6)) =");
    write(add(-100, 50, -25, 12, -6));
    writeln();

    write("(400 % 300 % 200) % (50 % 29 % 39 % 50) =");
    write(mod(400, 300, 200, 50, 29, 39));
    writeln();

    write("4200 / (-13) / 17 / ((-60) / 40) =");
    write(div(4200, -13, 17, -60, 40));
    writeln();

    write("2 * (-3) * 5 * ((-7) * 69) =");
    write(mul(2, -3, 5, -7, 69));
    writeln();

    write("mulSub(1, 2, 3, 5, 8, -13) =");
    write(mulSub(1, 2, 3, 5, 8, -13));
    writeln();
}
