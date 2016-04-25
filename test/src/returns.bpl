void voidsucceed(void) {
    write("voidsucceed");
    writeln();
}

string stringsucceed(void) {
    return "stringsucceed";
}

string ifelsesucceed(void) {
    if(1) {
        return "ifelsesucceed";
    } else {
        return "ifelsesucceed";
    }
}

string compoundsucceed(void) {
    int zero;
    int one;
    {
        int two[69];
        int three;
        {
            string four;
            string *five;
            {
                int six;
                int *seven;
                four = "compoundsucceed";
                {
                    string eight[42];
                    string nine;
                    five = &four;
                    {
                        return *five;
                    }
                }
            }
        }
    }
}

// int intfail(void) {
//     write("intfail");
//     writeln();
// }

// void voidfail(void) {
//     write("voidfail");
//     writeln();
//     return 5;
// }

void main(void) {
    voidsucceed();
    write(stringsucceed());
    writeln();
    write(ifelsesucceed());
    writeln();
    write(compoundsucceed());
    writeln();
    // intfail();
    // voidfail();
}
