//Alocação dinâmica
#include <stdlib.h>
#include <stdio.h>
//calloc, malloc, realloc, free
typedef struct {
    int numero;
    char letra;
} Alocacao;
//Não apenas com váriaveis convencionais 
void main() {
    char palavra[10] = {'p', 'o', 'n', 't', 'e', 'i', 'r', 'o', 's', '!'};
    int i;
    //Criando um ponteiro
    Alocacao *dinamica = NULL;
    //            Cast      Função     Argumento
    dinamica = (Alocacao *) calloc(10, sizeof(Alocacao));
    //recebe   (Tipo *....) calloc(N° de elementos, TAM deles)
    dinamica = (Alocacao *) malloc(5 * sizeof(Alocacao));
    //recebe   (Tipo *....) malloc(Tamanho de bytes)
    dinamica = (Alocacao *) realloc(dinamica, sizeof(Alocacao) * 10);
    //recebe   (Tipo *....) realloc(Ponteiro, tamanho de bytes)
    
    //IMPORTANTE!!!
    if (dinamica == NULL) exit(1); 

    for (i = 0; i < 10; i++) {
        dinamica[i].numero = i;
        dinamica[i].letra = palavra[i];
    }

    for (i = 0; i < 10; i++) printf("%d ", dinamica[i].numero); 
    for (i = 0; i < 10; i++) printf("%c", dinamica[i].letra);

  //free(ponteiro)
    free(dinamica);
}