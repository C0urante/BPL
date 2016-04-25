void switch(int arr[]) {
    int tmp;
    tmp = arr[0];
    arr[0] = arr[1];
    arr[1] = tmp;
}

void main(void) {
    int A[2];

    A[0] = 1;
    A[1] = 0;

    write(A[0]);
    writeln();

    write(A[1]);
    writeln();

    switch(A);

    write(A[0]);
    writeln();

    write(A[1]);
    writeln();
}
