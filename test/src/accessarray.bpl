// This is a line comment.

/* This is a block comment.
   This is still a block comment.
   This is the last line of a block comment. */

// Hello, world!
// How's it going?

void printIndex(int A[], int i) {
    write(A[i]); // Look at me, commenting next to actual code! Hope this compiles...
}

void main(void) {
    int arr[10];
    int i;
    i = 0;
    while(i < 10) {
        arr[i] = i * i;
        printIndex(arr, i);
        i = i + 1;
    }
}
