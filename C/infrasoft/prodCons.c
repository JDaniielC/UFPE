#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
 
#define NUMCONS 1
#define NUMPROD 2
#define BUFFERSIZE 1000
 
pthread_t cons[NUMCONS];
pthread_t prod[NUMPROD];
pthread_mutex_t buffer_mutex;
int buffer[BUFFERSIZE];
int currentidx;
sem_t buffer_full, buffer_empty;
 
 
 
void *produtor(void *arg) {
    int n;
    while(1) {
        n = rand()%1000;
        sem_wait(&buffer_full);
        pthread_mutex_lock(&buffer_mutex);
        buffer[currentidx++] = n;
        pthread_mutex_unlock(&buffer_mutex);
        sem_post(&buffer_empty);
        printf("Produzindo numero %d\n", n);
        sleep((1 + rand()%5));
    }
}
 
 
void *consumidor(void *arg) {
    int n;
    while(1) {
        sem_wait(&buffer_empty);
        pthread_mutex_lock(&buffer_mutex);
        n = buffer[--currentidx];
        pthread_mutex_unlock(&buffer_mutex);
        sem_post(&buffer_full);
        printf("Consumindo numero %d\n", n);
        sleep(1 + (rand()%5));
    }
}
 
 
 
 
int main(int argc, char **argv) {
    int i;    
    pthread_mutex_init(&buffer_mutex, NULL);
    sem_init(&buffer_full, 0, BUFFERSIZE);
    sem_init(&buffer_empty, 0, 0);
 
    for(i=0; i<NUMCONS; i++)
        pthread_create(&(cons[i]), NULL, consumidor, NULL);
 
    for(i=0; i<NUMPROD; i++)
        pthread_create(&(prod[i]), NULL, produtor, NULL);
 
    for(i=0; i<NUMCONS; i++)
        pthread_join(cons[i], NULL);
 
    for(i=0; i<NUMPROD; i++)
        pthread_join(prod[i], NULL);
}