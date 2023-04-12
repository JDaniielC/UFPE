#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define NUM_THREADS 2
#define LINHAS 3
#define COLUNAS 3

int A[3][3] = { {1, 2, 5}, {3, 4, 2}, {5, 6, 1} };
int B[3][3] = { {1, 2, 5}, {3, 4, 2}, {5, 6, 1} };
int R[3][3];

void *threadCode (void *tID) {
  int threadId = (*(int *)tID);
  for (int i = threadId; i < LINHAS; i = i+NUM_THREADS) {
    for (int j = 0; j < COLUNAS; j++) {
      R[i][j] = 0;
      for (int k = 0; k < COLUNAS; k++) {
        R[i][j] = A[i][j] + B[i][k];
      }
    }
  }
}

int main() {
  pthread_t threads[NUM_THREADS];
  int *taskIDs[NUM_THREADS];

  for (int i = 0; i < NUM_THREADS; i++) {
    taskIDs[i] = (int *) malloc(sizeof(int));
    *taskIDs[i] = i;
    pthread_create(&threads[i], NULL, threadCode, (void*) taskIDs[i]);
  }

  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }

  for (int i = 0; i < LINHAS; i++) {
    for (int j = 0; j < COLUNAS; j++) {
      printf("%d\t", R[i][j]);
    }
    printf("\n");
  }
}