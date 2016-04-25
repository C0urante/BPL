int g_int;
string g_string;
int g_arr[100];

int sixth(int arg[]) {
    write("sixth(arg)");
    writeln();
    return arg[5];
}

void populate(int arr[], int length) {
    int i;
    i = 0;
    write("populate(arr, length)");
    writeln();
    while(i < length) {
        arr[i] = i;
        i = i + 1;
    }
}

int mangledfive(void) {
    string s;
    int result[10];
    write("mangledfive()");
    writeln();
    s = "Hello, world!";
    write(s);
    writeln();
    populate(result, 10);
    return sixth(result);
}

void main(void) {
    int l_array[10];
    populate(l_array, 10);
    write(sixth(l_array));
    writeln();
    populate(g_arr, 100);
    write(sixth(g_arr));
    writeln();
    write(mangledfive());
    writeln();
}
