#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NUM_THREADS 5

void *printHello(void *threadId) {
  int tid = *((int*)threadId);
  printf("Olá, mundo! Sou eu, a thread #%d!\n", tid);
  pthread_exit(NULL);
}

int main() {
  pthread_t threads[NUM_THREADS];
  int *taskIds[NUM_THREADS];
  
  int rc;
  for (int i = 0; i < NUM_THREADS; i++) {
    taskIds[i] = (int *) malloc(sizeof(int));
    *taskIds[i] = i;
    printf("No main: criando thread %d\n", i);
    rc = pthread_create(&threads[i], NULL, printHello, (void *)taskIds[i]);
    if (rc) {
      printf("ERRO, código de retorno é %d\n", rc);
      exit(-1);
    }
  }
  pthread_exit(NULL);
}