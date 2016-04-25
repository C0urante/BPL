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
    return x / y / z / (x1/x2);
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

int orderTest(void) {
    int x;
    int y;
    int z;
    int T_T;
    y = (z = T_T = 5 + 4 * 3 < ---*x);
    hello();
    return y * 3 + 4;
    ;
    ;;;;
    ;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;
}

void main(void){}
