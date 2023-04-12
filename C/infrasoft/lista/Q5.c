#include <stdio.h>
#include <stdlib.h>
#include "pthread.h"

#define P 2
#define C 2

typedef struct elem{
  int value;
  struct elem *prox;
} Elem;

typedef struct blockingQueue{
  unsigned sizeBuffer, statusBuffer;
  Elem *head, *last;
} BlockingQueue;

// Pode ser iniciado estaticamente
pthread_t produtora[P];
pthread_t consumidora[C];

pthread_mutex_t mutex_prod = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex_cons = PTHREAD_MUTEX_INITIALIZER;
// Mutex para as condicionais:
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
// Não deixaram usar semáforo:
pthread_cond_t buffer_full = PTHREAD_COND_INITIALIZER;
pthread_cond_t buffer_empty = PTHREAD_COND_INITIALIZER;
// Funções da questão e rotinas
BlockingQueue* newBlockingQueue(unsigned inSizeBuffer);
void putBlockingQueue(BlockingQueue* Q, int newValue);
int takeBlockingQueue(BlockingQueue* Q);
void *rotinaProdutor(void *arg);
void *rotinaConsumidor(void *arg);

int main() {
  BlockingQueue *Queue = newBlockingQueue(10);
  for(int i = 0; i < P; i++) {
    printf("Thread[%d] produtora iniciada: \n", i);
    pthread_create(&(produtora[i]), NULL, rotinaProdutor, Queue);
  }

  for(int i = 0; i < C; i++) {
    printf("Thread[%d] consumidora iniciada: \n", i);
    pthread_create(&(consumidora[i]), NULL, rotinaConsumidor, Queue);
  }

  for(int i = 0; i < C; i++)
    pthread_join(consumidora[i], NULL);
  for(int i = 0; i < P; i++)
    pthread_join(produtora[i], NULL);

  pthread_mutex_destroy(&mutex_prod);
  pthread_mutex_destroy(&mutex_cons);
  pthread_mutex_destroy(&mutex);

  pthread_cond_destroy(&buffer_full);
  pthread_cond_destroy(&buffer_empty);

  pthread_exit (NULL);
}

void *rotinaProdutor(void *arg) {
  int n;
  BlockingQueue *fila = arg;
  while(1) {
    n = rand()%1000;
    if (fila->statusBuffer >= fila->sizeBuffer)  {
      printf("Fila cheia\n");
      // Trava a produção:
      pthread_cond_wait(&buffer_full, &mutex);
    }
    else {
      pthread_mutex_lock(&mutex_prod);
      putBlockingQueue(arg, n);
      pthread_mutex_unlock(&mutex_prod);
      // Destrava o consumo
      pthread_cond_signal(&buffer_empty);
      printf("Put: %d\n", n);
      // Pra poder ver o progresso
      sleep((1 + rand()%5));
    }
  }
}
 
void *rotinaConsumidor(void *arg) {
  int n;
  BlockingQueue *fila = arg;
  while(1) {
    if (fila->statusBuffer <= 0) {
      printf("Fila vazia\n");
      // Trava o consumo
      pthread_cond_wait(&buffer_empty, &mutex);
    } else {
      pthread_mutex_lock(&mutex_cons);
      n = takeBlockingQueue(arg);
      pthread_mutex_unlock(&mutex_cons);
      // Destrava a produção
      pthread_cond_signal(&buffer_full);
      printf("Take: %d\n", n);
      // Pra poder ver o progresso
      sleep(1 + (rand()%5));
    }
  }
}

//  Cria uma nova fila Bloqueante do tamanho do valor passado.
BlockingQueue* newBlockingQueue(unsigned inSizeBuffer) {
  BlockingQueue *newQueue = (BlockingQueue*) malloc(sizeof(BlockingQueue));
  if (newQueue == NULL) {
    printf("Falha ao alocar a fila\n");
  }
  newQueue->sizeBuffer = inSizeBuffer;
  newQueue->statusBuffer = 0;
  newQueue->head = NULL;
  newQueue->last = NULL;
  return newQueue;
}
// Insere um elemento no final da fila bloqueante Q, bloqueando 
// a thread que está inserindo, caso a fila esteja cheia.
void putBlockingQueue(BlockingQueue* Q, int newValue) {
  Q->statusBuffer++;
  printf("QTD: %d\n", Q->statusBuffer);
  // Criar novo nó
  Elem *N = (Elem *) malloc(sizeof(Elem));
  if (N == NULL) {
    printf("Error na criacao do no\n");
  }
  N->value = newValue;

  if (Q->head == NULL) Q->head = N;
  else Q->head->prox = N;
  
  N->prox = (Q->last == NULL) ? NULL : Q->last;
  Q->last = N;
}
// retira o primeiro elemento da fila bloqueante Q, bloqueando 
// a thread que está retirando, caso a fila esteja vazia.
int takeBlockingQueue(BlockingQueue* Q) {
  int retorno;
  Elem *p = NULL;
  Q->statusBuffer--;
  printf("QTD: %d\n", Q->statusBuffer);
  p = Q->head;
  if (p == NULL) {
    printf("Error: Queue is empty\n");
    return 0;
  }
  Q->head = p->prox;
  retorno = p->value;
  return retorno;
}
