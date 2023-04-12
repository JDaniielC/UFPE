#include <iostream>

using namespace std;

namespace meuNamespace {
    int (*input)(const char *, ...) = std::scanf;
    int (*output)(const char *, ...) = std::printf;
};

using namespace meuNamespace;

int main() {
    output("Funciona como printf");
}