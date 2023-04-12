#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

long contador = 0;
pthread_mutex_t myMutex = PTHREAD_MUTEX_INITIALIZER;

void *inc (void *threadID) {
  while (pthread_mutex_lock(&myMutex) != 0) {;}
  contador++;
  pthread_mutex_unlock(&myMutex);
}

void *dec (void *threadID) {
  while (pthread_mutex_lock(&myMutex) != 0) {;}
  contador--;
  pthread_mutex_unlock(&myMutex);
}

int main () {
  pthread_t thread1, thread2;
  pthread_create(&thread1, NULL, inc, NULL);
  pthread_create(&thread2, NULL, dec, NULL);
  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);
  printf("Valor final do contador: %ld\n", contador);
  pthread_exit(NULL);
}
