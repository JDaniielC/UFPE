#include <stdio.h> 
#include <pthread.h> 
#include <semaphore.h> 
#include <unistd.h> 
 
sem_t mutex; 
 
void* thread(void* arg) { 
	long id = (intptr_t)arg;
	//wait 
	sem_wait(&mutex); 
	printf("Thread %ld: Entrando..\n", arg); 
 
	//critical section 
	sleep(4); 
 
	//signal 
	printf("Thread %ld: Saindo...\n", arg); 
	sem_post(&mutex); 
} 
 
 
int main() { 
	sem_init(&mutex, 0, 1); 
	pthread_t t1,t2; 
	long id = 1;
	pthread_create(&t1,NULL,thread,(void*)(intptr_t)id); 
	sleep(2); 
	id = 2;
	pthread_create(&t2,NULL,thread,(void *)(intptr_t)id); 
	pthread_join(t1,NULL); 
	pthread_join(t2,NULL); 
	sem_destroy(&mutex); 
	return 0; 
}