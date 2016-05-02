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
    int f;
    int u;
    int c;
    int k;

    int s;
    int h;
    int i;
    int t;

    int stack;

    f = 7485;
    u = 4344;
    c = -19964;
    k = -10450;
    s = -6748;
    h = 19796;
    i = -6745;
    t = 24591;
    stack = 24201;

    write(s - c / i / k + t + stack * u * h / f);
    write("?=");
    write((-6748) - (-19964) / (-6745) / (-10450) + 24591 + 24201 * 4344 * 19796 / 7485);
    write("?=");
    write((-6748) - (-19964) / i / (-10450) + t + stack * 4344 * h / 7485);
    writeln();

    f = -16640;
    u = 17403;
    c = 446;
    k = 24088;
    s = -21307;
    h = -6903;
    i = 16655;
    t = -20185;
    stack = 1043;

    write(stack + i * h + u * s + f / c - k + t);
    write("?=");
    write(1043 + 16655 * (-6903) + 17403 * (-21307) + (-16640) / 446 - 24088 + (-20185));
    write("?=");
    write(stack + 16655 * h + u * s + (-16640) / 446 - k + (-20185));
    writeln();

    f = -9528;
    u = -12772;
    c = -18290;
    k = -5628;
    s = 24368;
    h = 23037;
    i = -13513;
    t = -20776;
    stack = -43;

    write(f + t - c * h - u / i * k - s - stack);
    write("?=");
    write((-9528) + (-20776) - (-18290) * 23037 - (-12772) / (-13513) * (-5628) - 24368 - (-43));
    write("?=");
    write(f + (-20776) - (-18290) * h - (-12772) / i * (-5628) - 24368 - stack);
    writeln();

    f = 12788;
    u = -9081;
    c = 27394;
    k = -982;
    s = -24129;
    h = -2467;
    i = -30649;
    t = 9126;
    stack = -5741;

    write(c * t + k + u * s - h / f - i - stack);
    write("?=");
    write(27394 * 9126 + (-982) + (-9081) * (-24129) - (-2467) / 12788 - (-30649) - (-5741));
    write("?=");
    write(27394 * 9126 + k + u * (-24129) - (-2467) / f - (-30649) - (-5741));
    writeln();

    f = -1125;
    u = 14808;
    c = -18765;
    k = -18322;
    s = -12349;
    h = -2486;
    i = 21939;
    t = -26614;
    stack = -28163;

    write(h / c - i - k * t - stack + u + s / f);
    write("?=");
    write((-2486) / (-18765) - 21939 - (-18322) * (-26614) - (-28163) + 14808 + (-12349) / (-1125));
    write("?=");
    write((-2486) / (-18765) - i - k * (-26614) - stack + 14808 + s / f);
    writeln();
}
