int n1;
string s1;
int a1[12];
string s2;
int a2[5];
string *p1;
int n2;
string a3[0];
int *p2;
string s3;

void populate_a2(void) {
    int i;
    i = 0;
    while(i < 5) {
        a2[i] = i * i;
        i = i + 1;
    }
}

void printarray(int arr[], int length) {
    int i;
    i = 0;
    while(i < length) {
        write("arr[");
        write(i);
        write("] =");
        write(arr[i]);
        writeln();
        i = i + 1;
    }
}

void main(void) {
    n1 = 5;
    write(n1);
    writeln();

    s1 = "Hello, world!";
    write(s1);
    writeln();

    a1[3] = 64000000;
    write("a1[3] =");
    write(a1[3]);
    writeln();

    a1[2] = 69;
    write("a1[2] =");
    write(a1[2]);
    writeln();

    a1[1] = 42;
    write("a1[1] =");
    write(a1[1]);
    writeln();

    populate_a2();
    printarray(a2, 5);
}
