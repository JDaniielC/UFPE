#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
 
#define NUM_THREADS  3
#define COUNT_LIMIT 12
 
int     count = 0;
pthread_mutex_t count_mutex;
pthread_cond_t count_threshold_cv;
 
void *inc_count(void *t) {
  int i;
  int my_id = (intptr_t)t;
 
  for (i=0; i < 10; i++) {
    pthread_mutex_lock(&count_mutex);
    count++;
 
    if (count == COUNT_LIMIT) {
      printf("inc_count(): thread %ld, count = %d  limite alcançado. ",my_id, count);
      pthread_cond_signal(&count_threshold_cv);
      printf("enviando sinal.\n");
      }
    printf("inc_count(): thread %ld, count = %d, liberando mutex\n",my_id, count);
    pthread_mutex_unlock(&count_mutex);
 
    }
  pthread_exit(NULL);
}
 
void *watch_count(void *t) {
  int my_id = (intptr_t)t;
 
  printf("Iniciando watch_count(): thread %ld\n", my_id); 
 
  pthread_mutex_lock(&count_mutex);
  while (count < COUNT_LIMIT) {
    printf("watch_count(): thread %ld Count= %d. esperando...\n", my_id,count);
    pthread_cond_wait(&count_threshold_cv, &count_mutex);
    printf("watch_count(): thread %ld Sinal da varável de condição recebido. Count= %d\n", my_id,count);
    }
  printf("watch_count(): thread %ld alterando o valor de count...\n", my_id);
  count += 125;
  printf("watch_count(): thread %ld count = %d.\n", my_id, count);
  printf("watch_count(): thread %ld Liberando mutex.\n", my_id);
  pthread_mutex_unlock(&count_mutex);
  pthread_exit(NULL);
}
 
int main(int argc, char *argv[])
{
  int i, rc; 
  long t1=1, t2=2, t3=3;
  pthread_t threads[3];
 
  pthread_mutex_init(&count_mutex, NULL);
  pthread_cond_init (&count_threshold_cv, NULL);
 
  pthread_create(&threads[0], NULL, watch_count, (void *)(intptr_t)t1);
  pthread_create(&threads[1], NULL, inc_count, (void *)(intptr_t)t2);
  pthread_create(&threads[2], NULL, inc_count, (void *)(intptr_t)t3);
 
  for (i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }
  printf ("Main(): valor final de count = %d. Done.\n", NUM_THREADS, count);
 
  pthread_mutex_destroy(&count_mutex);
  pthread_cond_destroy(&count_threshold_cv);
  pthread_exit (NULL);
 
}