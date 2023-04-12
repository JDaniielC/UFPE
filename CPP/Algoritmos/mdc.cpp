#include <iostream>

using namespace std;

int mdc(int a, int b) {
    int c;
    while (b != 0) {
        c = a % b;
        a = b;
        b = c;
    }
    return a;
}

int main () {
    int a = 168, b = 65;
    int resultado = mdc(a, b);
    cout << "Resultado " << resultado << endl;
}