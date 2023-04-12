#include <iostream>

using namespace std;

class DC {
    private: 
        float *f;
    public:
    //Constructor:
        DC(float F) {
            f = (float *) malloc (sizeof(float));
            *f = F;
            cout << "Contruido\n";
        }
    //Desctructor:
        ~DC() {
            if(f != nullptr) free(f);
            cout << "Destruido\n"; 
        }

        void mostrar() {
            cout << *f << "\n";
        }
};

class op {
    public:
        int x;
        op (int a) {
            x = a;
        }
    //Foi necessário criar esse método:
    //note:   candidate expects 1 argument, 0 provided
        op () {
            return;
        }
    //Operações:
        op operator + (op f) {
            op result;
            result.x = this->x + f.x;
            return result;
        }

        op operator - (op f) {
            op result;
            result.x = this->x - f.x;
            return result;
        }
};

int main() {
    DC v1(2);
    DC v2(3.57);

    v1.mostrar();
    v2.mostrar();

    op a(7), b(3);
    op soma = a + b;
    op sub = a - b;

    cout << "Soma: " << soma.x << endl;
    cout << "Sub: " << sub.x << endl;
    
    //Ao acabar o programa ele destrói automaticamente
    //Para apagar antes, use delete variável;
}