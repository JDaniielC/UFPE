#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define QTD_VETOR 10
// Professor deixou criar vetor pronto.
int vetor[10] = {20, 30, 54, 2, 109, 1000, 1, 0, 234, 28};

typedef struct {
  int *vetor, esquerda, direita;
} Args;

void *qSort(void *args);
int partition(int arr[], int p, int r);
void swap(int *a, int *b);

int main() {
  Args props = {vetor, 0, QTD_VETOR - 1};
  printf("Vetor desordenado: 20, 30, 54, 2, 109, 1000, 1, 0, 234, 28\n");
  qSort(&props);
  printf("Vetor ordenado:     ");
  for (int i = 0; i < QTD_VETOR; i++) printf("%d, ", vetor[i]);
}
// Rotina:
void *qSort(void *args) {
  Args *props = args;
  Args aux = {props->vetor, props->esquerda, props->direita};
  if (aux.esquerda < aux.direita) {
    pthread_t threadEsquerda, threadDireita;
    int j = partition(aux.vetor, aux.esquerda, aux.direita);

    Args arg1 = {aux.vetor, aux.esquerda, j-1};
    Args arg2 = {aux.vetor, j+1, aux.direita};
    // A mágica acontece aqui:
    pthread_create (&threadEsquerda, NULL, qSort, &arg1);
    pthread_create (&threadDireita, NULL, qSort, &arg2);
    
    pthread_join(threadEsquerda, NULL);
    pthread_join(threadDireita, NULL);
  }
}

void swap(int *x, int *y) {
  int temp = *x; *x = *y; *y = temp;
}
// Algoritmo de partição padrão:
int partition(int vet[], int inicio, int final) {
  int pivo = inicio + inicio%(final - inicio + 1);
  int ref = vet[pivo];
  int i = inicio - 1;
  swap(&vet[pivo], &vet[final]);
  for (int j = inicio; j < final; j++) {
    if (vet[j] < ref) {
      i++;
      swap(&vet[i], &vet[j]);
    }
  }
  swap(&vet[i+1], &vet[final]);
  return i + 1;
}
