#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

// Definindo variáveis que serão utilizadas.
#define NUM_THREADS 4
#define CHUNK_SIZE 2
#define INICIO 0
#define PASSO 1
#define FINAL 15

typedef struct {
    int inicio;
    int passo;
    int final;
    int thread_index;
    void (*f)(int);
} Args;

pthread_t THREADS[NUM_THREADS];

void printInteracoes(int i);
void *loop(void *args);
int schedule_static(int chunk_size, int num_threads, int iteracoes_restantes);
int schedule_dynamic(int chunk_size, int iteracoes_restantes);
int schedule_guided(int chunk_size, int num_threads, int iteracoes_restantes);
void omp_for( int inicio, int passo, int final, int schedule, int chunk_size, void (*f)(int));

int main() {
    // Perguntar ao usuário o tipo de escalonador
    int scheduleType;
    printf("Escolha o tipo de escalonador:\n1: Estatico\n2: Dinamico\n3: Guiado\n");
    scanf("%d", &scheduleType);
    void (*f)() = printInteracoes;
    omp_for(INICIO, PASSO, FINAL, scheduleType, CHUNK_SIZE, f);
}

void printInteracoes(int i) {
    printf("%d\n", i);
}

int schedule_static(int chunk_size, int num_threads, int iteracoes_restantes) {
    if (chunk_size * num_threads < iteracoes_restantes) return chunk_size;
    int retorno = iteracoes_restantes / num_threads; // Evitando erros
    return retorno == 0 ? iteracoes_restantes % num_threads : retorno;
}

int schedule_dynamic(int chunk_size, int iteracoes_restantes) {
    if (iteracoes_restantes >= chunk_size) return chunk_size;
    return iteracoes_restantes % chunk_size;
}

int schedule_guided(int chunk_size, int num_threads, int iteracoes_restantes) {
    if (ceil(iteracoes_restantes / num_threads) < chunk_size ) return chunk_size;
    float retorno = ceil((float)iteracoes_restantes / (float)num_threads);
    return (int)retorno;
}

// For para ser a rotina das threads:
void *loop(void *args) {
// Como a questão não exigiu exclusão mútua, evitei regiões críticas.
    Args *props = (Args*) args;
    // Definindo variáveis:
    int inicio = props->inicio;
    int final = props->final;
    int index = props->thread_index;
    int passo = props->passo;
    // for padrão:
    if (inicio >= final) pthread_join(THREADS[index], NULL);
    (*props).f(inicio);
    inicio += passo;
    loop(args);
}

void omp_for(int inicio, int passo, int final, int schedule, int chunk_size, void (*f)(int)) {
    int thread, iteracoes_restantes = final;
    Args *props = (Args*) malloc(sizeof(Args));
    props->f = printInteracoes;
    props->inicio = inicio;
    props->passo = passo;
    props->final = final;

    if (schedule == 1) {
        printf("Foi escolhido o escalonador estatico.\n");

        for (thread = 0; iteracoes_restantes > 0; thread++) {
            int chunk = schedule_static(chunk_size, NUM_THREADS, iteracoes_restantes);
            for (int i = 0; i < NUM_THREADS; i++){
                iteracoes_restantes -= chunk;
                props->final = inicio + chunk;
                pthread_create(&THREADS[thread%NUM_THREADS], NULL, loop, props);
                props->inicio = props->final;
                if (inicio >= final - 1) i = NUM_THREADS;
                // Evitando erros
            }
            if (chunk == 0) iteracoes_restantes = 0;
        }
    } else if (schedule == 2) {
        printf("Foi escolhido o escalonador dinamico.\n");

        for (int thread = 0; iteracoes_restantes > 0; thread++) {
            props->thread_index = thread % NUM_THREADS;
            printf("Thread[%d]: ", props->thread_index);
            int chunk = schedule_dynamic(chunk_size, iteracoes_restantes);
            iteracoes_restantes -= chunk;
            props->final = inicio + chunk;
            pthread_create(&THREADS[thread%NUM_THREADS], NULL, loop, props);
            props->inicio = props->final;
            // Evitando erros
            if (chunk == 0) iteracoes_restantes = 0;
        }

    } else if (schedule == 3) {
        printf("Foi escolhido o escalonador guiado.\n");
        
        for (int thread = 0; iteracoes_restantes > 0; thread++) {
            props->thread_index = thread % NUM_THREADS;
            printf("Thread[%d]: ", props->thread_index);
            int chunk = schedule_guided(chunk_size, NUM_THREADS, iteracoes_restantes);
            iteracoes_restantes -= chunk;
            props->final = inicio + chunk;
            pthread_create(&THREADS[thread%NUM_THREADS], NULL, loop, props);
            props->inicio = props->final;
            // Evitando erros
            if (chunk == 0) iteracoes_restantes = 0;
        }

    } else {
        printf("Escalonador inválido\n");
        printf("Retornando ao main:\n");
        main();
    }
}

//  Como funciona: for(int i = inicio ; i < final ; i += passo ){f(i);}

// printf("Foi escolhido o escalonador estático.\n");
// int iteracoes_restantes = final;
// while (iteracoes_restantes > 0) {
//     int chunk = schedule_static(chunk_size, NUM_THREADS, iteracoes_restantes);
//     printf("Chuck: %d\n", chunk);
//     for (int thread = 0; thread < NUM_THREADS; thread++) {
//         for (int iteracoes = 0; iteracoes < chunk; iteracoes++) {
//             iteracoes_restantes--;
//             printf("Thread[%d]: ", thread%NUM_THREADS);
//             (*f)(inicio);
//             inicio++;
//         }
//     }
// }

// printf("Foi escolhido o escalonador dinamico.\n");
// int iteracoes_restantes = final;
// for (int thread = 0; iteracoes_restantes > 0; thread++) {
//     int chunk = schedule_dynamic(chunk_size, iteracoes_restantes);
//     printf("Chuck: %d\n", chunk);
//     for (int iteracoes = 0; iteracoes < chunk; iteracoes++) {
//         iteracoes_restantes--;
//         printf("Thread[%d]: ", thread%NUM_THREADS);
//         (*f)(inicio);
//         inicio++;
//     }  
// }   