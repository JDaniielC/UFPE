#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define ITERACOES 25 // A partir daqui não muda
#define NUM_THREADS 2
#define X 2 // N de incógnitas

typedef struct {
  int iteracoes;
  int id;
} ThreadProps;

ThreadProps propsThread[NUM_THREADS];
pthread_t threads[NUM_THREADS];
pthread_barrier_t barrier;

int matriz[NUM_THREADS][X] = {0};
// Matrizes dadas na questão:
int matrizA[2][2] = {{2, 1}, {5, 7}};
int matrizB[2] = {11, 13}; 

double SOLUCAOPARCIAL[ITERACOES + 1][X];

// Funções
double somatorio(int i , int ITERACAO);
void *rotina(void *threadid);

int main() {
  pthread_barrier_init(&barrier, NULL, NUM_THREADS);

  int preenchimento = X/NUM_THREADS;
  for(int i = 0, j = 0, p = 0; i < NUM_THREADS; i++, p = 0){
    if(i != (NUM_THREADS - 1)){
      while(p < preenchimento){
        matriz[i][j] = 1;
        j++;   p++;
      }
    }
    else {
      while(j <= X){
        matriz[i][j] = 1;
        j++;
      }
    }
  }

  // Assumindo que as soluções inicia em 1
  for(int i = 0 ; i < ITERACOES + 1 ; i++) {
    for(int j = 0 ; j < X ; j++) SOLUCAOPARCIAL[i][j] = 1.0;  
  }

  for(int i = 0; i < NUM_THREADS; i++){
    propsThread[i].id = i;
    propsThread[i].iteracoes = 0;
    pthread_create(&threads[i], NULL, rotina, (void *)&propsThread[i]);
  }
    
  for(int i = 0; i < NUM_THREADS ; i++){
    pthread_join(threads[i], NULL);
  } 

  for(int i = 0; i < X; i++)
    printf("x%d = %lf\n", i, SOLUCAOPARCIAL[ITERACOES-1][i]);

  pthread_barrier_destroy(&barrier);
  pthread_exit(NULL);
}

double somatorio(int i , int ITERACAO){
  double soma = 0;
  for(int j = 0 ; j < X ;j++)
    if(i != j) soma += ((double)matrizA[i][j])*SOLUCAOPARCIAL[ITERACAO - 1][j];
  return soma;
}

void *rotina(void *args) {
  ThreadProps *props = args;
  int solucao = props->iteracoes;
  int id = props->id;
  double A, soma;
  printf("Thread[%d] trabalhando\n", id);
  for(;solucao != ITERACOES ;){
    for(int j = 0 ; j < NUM_THREADS; j++){
      if(matriz[id][j] == 1){
        // Exemplo de execução
        soma = somatorio(j, solucao);
        A = (1 / (double)matrizA[j][j]);
        SOLUCAOPARCIAL[solucao][j] = A * ((double) matrizB[j] - soma);
      }   
    }
    solucao++;
    pthread_barrier_wait(&barrier);
  }
} 