void main(void) {
    int arr[4];
    string sarr[5];
    int i;

    arr[0] = 69;
    arr[1] = 42;
    arr[2] = -42;
    arr[3] = -69;

    i = 0;
    while(i < 4) {
        write(arr[i]);
        i = i + 1;
    }
    writeln();

    sarr[0] = "Hello";
    sarr[1] = "world!";
    sarr[2] = "How's";
    sarr[3] = "it";
    sarr[4] = "going?";
    i = 0;
    while(i < 5) {
        write(sarr[i]);
        i = i + 1;
    }
}
