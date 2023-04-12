//Ponteiros e funções
#include <stdio.h>

// * Para devolver ponteiro
int *atribuir(int *numero) {
    int *ponteiro = NULL;
    //O mesmo que fazer no main: ponteiro = &numero;
    ponteiro = numero;
    //Devolver ponteiro
    return ponteiro;
}

void mostrar(int *ponteiro) {
    printf("Ponteiro: %p, %d\n", ponteiro, *ponteiro);
}

void funcao(int (*p)(const char *, ...), int *ponteiro);

int main() {
    //Criando Ponteiro:
    int *ponteiro = NULL, numero = 10;
//  Ponteiro recebe o endereço de memoria de numero
    ponteiro = atribuir(&numero);

    printf("Numero: %p, %d\n", &numero, numero);
    mostrar(ponteiro);
    mostrar(&numero);

    //Ponteiro para função
    int (*ponteiroFunc) () = NULL;
    ponteiroFunc = printf;

    (*ponteiroFunc)("Numero: %p, %d\n", &numero, numero);
    funcao(ponteiroFunc, ponteiro);
    funcao(ponteiroFunc, &numero);

    return 0;
}

void funcao(int (*p)(const char *, ...), int *ponteiro) {
    (*p)("Ponteiro: %p, Valor: %d\n", ponteiro, *ponteiro);
}
