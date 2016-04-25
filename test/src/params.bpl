// Increments every element by one, and returns the sixth.
int sixth(int a[], int length) {
    int i;
    i = 0;
    while(i < length) {
        a[i] = a[i] + 1;
        i = i + 1;
    }
    return a[5];
}

// Sets every element equal to its indexes parity
void four(int a[], int length) {
    int i;
    i = 0;
    while(i < length) {
        a[i] = i % 2;
        i = i + 1;
    }
}

void writeall(int a[], int length, string begin, string middle, string end) {
    int i;
    i = 0;
    while(i < length) {
        write(begin);
        write(i);
        write(middle);
        write(a[i]);
        write(end);
        writeln();
        i = i + 1;
    }    
}

void main(void) {
    int arr[10];
    four(arr, 10);
    sixth(arr, 10);
    writeall(arr, 10, "arr[", "] =", "");
}
