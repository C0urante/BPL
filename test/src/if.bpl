void main(void) {
    write("You should see one line stating that 'if' is working, and two that 'if-else' is working.");
    writeln();

    if(1) {
        write("If: working");
        writeln();
    }
    if(0) {
        write("If: not working");
        writeln();
    }

    if(1) {
        write("If-else: working");
        writeln();
    } else {
        write("If-else: not working");
        writeln();
    }

    if(0) {
        write("If-else: not working");
        writeln();
    } else {
        write("If-else: working");
        writeln();
    }
}
