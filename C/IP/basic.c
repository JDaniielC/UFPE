#include <stdio.h>
int main() {
   int N, X;
   scanf("%d" , &N);
   scanf("%d", &X);

    int vetor[N]; //declarar o vetor original

   while( N != -1) {
    scanf("%d", &vetor[N]);
   }
   // copiar o ventororiginal em vetor antigo
   int vetorOriginal[N];

   for(int k = 0; k <N; k++) {
    vetorOriginal[k] = vetor[k];
   }
   //ordenar os numeros do vetor original
   int aux;
   for(int x = 0; x < N; x++) {
        for(int y = 0; y < N; y++) {

            if( vetor[x] > vetor[y]) {
                aux = vetor[x];
                vetor[x] = vetor[y];
                vetor[y] = aux;
            }
        }
    }

    int contador = 0;

    for(int j = 0; j < N; j++) {

        if(vetor[j] >= vetorOriginal[j]) {
            while(vetor[j] - vetorOriginal[j] == X) {
            contador = contador + 1;
            printf("%d\n", contador);
            printf("%d %d %d", vetorOriginal[j], vetor[j], j);
            }
        }

        if (vetorOriginal[j] > vetor[j]) {
            while(vetorOriginal[j] - vetor[j] == X) {
            contador = contador + 1;
            printf("%d\n", contador);
            printf("%d %d %d", vetorOriginal[j], vetor[j], j);
            }
        }
    }
   return 0;
}