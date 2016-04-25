string g_raw;
string *g_ptr;
string g_arr[2];

void main(void) {
    string tmp;
    string l_raw;
    string *l_ptr;
    string l_arr[2];

    g_raw = "This is a global string!";
    l_raw = "This is a local string!";

    write(g_raw);
    writeln();

    write(l_raw);
    writeln();

    tmp = "This is a dereferenced local string pointer!";
    l_ptr = &tmp;
    write(*l_ptr);
    writeln();

    g_ptr = &tmp;
    *g_ptr = "This is a dereferenced global string pointer!";
    write(*g_ptr);
    writeln();

    l_arr[0] = "This is the first element of a local string array!";
    l_arr[1] = "This is the second element of a local string array!";
    write(l_arr[0]);
    writeln();
    write(l_arr[1]);
    writeln();

    g_arr[0] = "This is the first element of a global string array!";
    write(g_arr[0]);
    writeln();
    g_arr[1] = "This is the second element of a global string array!";
    write(g_arr[1]);
    writeln();
}
