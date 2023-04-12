// Esse código é um exemplo do problema do jantar dos filósofos, nesse problema tem uma mesa de jantar com N filósofos e N garfos, o filósofo faz duas coisas: pensa e come, para comer ele precisa pegar os dois garfos mais próximos dele. Dois grandes problemas desse cenário é caso todos os filósofos peguem um garfo e ninguém mais consegue comer (deadlock) ou se um filósofo nunca consegue pegar os dois garfos e morre de fome (starvation). A implementação abaixo utiliza a idéia de todos os filósofos pegarem primeiro o garfo da direita e depois da esquerda com exceção do último que pega na ordem contrária, isso evita o deadlock mas não starvation.

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
 
typedef struct {
    pthread_mutex_t *fork_lft, *fork_rgt;
    const char *name;
    pthread_t thread;
} philosopher;
 
void* philosopherfunction(void* p) {
    philosopher *p_ptr = (philosopher*)p;
 
    while (1) {
        printf("%s is thinking\n", p_ptr->name);
        sleep(1+ rand()%5);
 
        printf("%s is hungry\n", p_ptr->name);
 
        pthread_mutex_lock(p_ptr->fork_lft);
        pthread_mutex_lock(p_ptr->fork_rgt);
 
        printf("%s is eating\n", p_ptr->name);
        sleep(1+ rand() % 8);
        pthread_mutex_unlock(p_ptr->fork_rgt);
        pthread_mutex_unlock(p_ptr->fork_lft);
        printf("%s finished eating\n", p_ptr->name);
 
    }
 
    return NULL;
}
 
int main()
{
    const char *nameList[] = { "Kant", "Guatma", "Russel", "Aristotle", "Bart" };
    pthread_mutex_t forks[5];
    philosopher** philosophers = malloc(sizeof(*philosophers)*5);
    int i;
 
    for (i = 0; i < 5; i++) {
        pthread_mutex_init(&forks[i], NULL);
    }
 
    for (i = 0; i < 4; i++) {
        philosophers[i] = malloc(sizeof(*philosophers[i]));
        philosophers[i]->name = nameList[i];
        philosophers[i]->fork_lft = &forks[i];
        philosophers[i]->fork_rgt = &forks[(i+1)%5];
        pthread_create( &philosophers[i]->thread, NULL, philosopherfunction, philosophers[i]);
    }
    //O ultimo filósofo pega os garfos na ordem contrária
    philosophers[i] = malloc(sizeof(*philosophers[i]));
    philosophers[i]->name = nameList[i];
    philosophers[i]->fork_lft = &forks[(i+1)%5];
    philosophers[i]->fork_rgt = &forks[i];
    pthread_create( &philosophers[i]->thread, NULL, philosopherfunction, philosophers[i]);
 
    pthread_exit(NULL);
    return 0;
}