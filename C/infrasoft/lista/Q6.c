#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

// ATENÇÃO PARA COMPILAR É NECESSÁRIO COLOCAR -lm
// POIS UTILIZA FUNÇÃO CEIL()

// Definindo variáveis que serão utilizadas.
#define NUM_THREADS 4
#define CHUNK_SIZE 2
#define INICIO 0
#define PASSO 1
#define FINAL 15
// Struct para escalonamento
typedef struct {
    int *partition;
    int num_partition;
} Divisions;
// Struct para argumentos da rotina
typedef struct {
    int inicio;
    int passo;
    int final;
    int thread_index;
    void (*f)(int);
} Args;

pthread_t THREADS[NUM_THREADS];
pthread_mutex_t MUTEX[NUM_THREADS];
int CHUNKS[25];
Divisions partition = {CHUNKS, 0};
// Funções da questão
void omp_for( int inicio, int passo, int final, int schedule, int chunk_size, void (*f)(int));
void printInteracoes(int i);
void *loop(void *args);
Divisions *schedule_static(int chunk_size, int num_threads, int iteracoes_restantes);
Divisions *schedule_dynamic(int chunk_size, int iteracoes_restantes);
Divisions *schedule_guided(int chunk_size, int num_threads, int iteracoes_restantes);

int main() {
    for(int i = 0; i < NUM_THREADS; i++) {
        pthread_mutex_init(&MUTEX[i], NULL);
    }
    // Perguntar ao usuário o tipo de escalonador
    int scheduleType;
    printf("Escolha o tipo de escalonador:\n1: Estatico\n2: Dinamico\n3: Guiado\n");
    scanf("%d", &scheduleType);
    void (*f)() = printInteracoes;
    omp_for(INICIO, PASSO, FINAL, scheduleType, CHUNK_SIZE, f);
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_mutex_destroy(&MUTEX[i]);
    }
}

void printInteracoes(int i) {
    printf("%d\n", i);
}

Divisions *schedule_static(int chunk_size, int num_threads, int iteracoes_restantes) {
    while (iteracoes_restantes > 0) {
        partition.num_partition++;
        // Cálculo do escalonador estático:
        if (chunk_size * num_threads < iteracoes_restantes)
            partition.partition[partition.num_partition - 1] = chunk_size;
        else {
            int retorno = iteracoes_restantes / num_threads; // Evitando erros
            if (retorno == 0) 
                retorno = iteracoes_restantes % num_threads;
            partition.partition[partition.num_partition - 1] = retorno;
        }
        // Evitando erros
        if (partition.partition[partition.num_partition - 1] == 0) partition.num_partition--;
        iteracoes_restantes -= partition.partition[partition.num_partition - 1];
    }
    // Retorna a variável configurada:
    return &partition;
}

Divisions *schedule_dynamic(int chunk_size, int iteracoes_restantes) {
    while (iteracoes_restantes > 0) {
        partition.num_partition++;
        // Cálculo para escalonador dinâmico
        if (iteracoes_restantes >= chunk_size) 
            partition.partition[partition.num_partition - 1] = chunk_size;
        else 
            partition.partition[partition.num_partition - 1] = iteracoes_restantes % chunk_size;
        // Evitando erros
        if (partition.partition[partition.num_partition - 1] == 0) partition.num_partition--;
        iteracoes_restantes -= partition.partition[partition.num_partition - 1];
    }
    // Retorna a variável configurada:
    return &partition;
}

Divisions *schedule_guided(int chunk_size, int num_threads, int iteracoes_restantes) {
    while (iteracoes_restantes > 0) {
        partition.num_partition++;
        // Cálculo para escalonador guiado:
        if (ceil(iteracoes_restantes / num_threads) < chunk_size ) 
            partition.partition[partition.num_partition - 1] = chunk_size;
        else {
            float retorno = ceil((float)iteracoes_restantes / (float)num_threads);
            partition.partition[partition.num_partition - 1] = (int)retorno;
        }
        // Evitando erros
        if (partition.partition[partition.num_partition - 1] == 0) partition.num_partition--;
        iteracoes_restantes -= partition.partition[partition.num_partition - 1];
    }
    // Retorna a variável configurada:
    return &partition;
}

// For para ser a rotina das threads:
void *loop(void *args) {
    Args *props = args;
    // for padrão:
    if (props->inicio >= props->final)
        pthread_exit(&THREADS[props->thread_index]);
    // Vai que né
    pthread_mutex_lock(&MUTEX[props->thread_index]);
    (*props).f(props->inicio);
    pthread_mutex_unlock(&MUTEX[props->thread_index]);

    props->inicio += props->passo;
    loop(args);
}

void omp_for(int inicio, int passo, int final, int schedule, int chunk_size, void (*f)(int)) {
    if (schedule == 1) {
        printf("Como funciona o escalonador estatico.\n");
        Divisions *chunks = schedule_static(chunk_size, NUM_THREADS, final);
        int copy_num_partition = chunks->num_partition;
        Args props[30] = {inicio, 1, final, 0, f};
        float ondas = ceil((float)copy_num_partition / (float)NUM_THREADS);
        int start = inicio;

        // Configurando os argumentos de cada thread:
        for (int thread = 0; thread < copy_num_partition; thread++){
            props[thread].thread_index = thread % NUM_THREADS;
            props[thread].final = props[thread].inicio + chunks->partition[thread];
            props[thread].passo = 1;
            start = props[thread].final;
            if (thread != copy_num_partition) // Pegando o inicio da próxima:
                props[thread + 1].inicio = start;

            printf("Thread[%d] - Inicio: %d - Final: %d\n", props[thread].thread_index, props[thread].inicio, props[thread].final);
        }
        for (int k = 0; k < copy_num_partition; k++) {
            pthread_create(&THREADS[k % NUM_THREADS], NULL, loop, &props[k]);
        }
        for (int k = 0; k < copy_num_partition; k++) {
            pthread_join(THREADS[k], NULL);
        }
    } else if (schedule == 2) {
        printf("Como funciona o escalonador dinamico.\n");
        Divisions *chunks = schedule_dynamic(chunk_size, final);
        int copy_num_partition = chunks->num_partition;
        Args props[30] = {inicio, 1, final, 0, f};
        float ondas = ceil((float)copy_num_partition / (float)NUM_THREADS);
        int start = inicio;
        // Configurando os argumentos de cada thread:
        for (int thread = 0; thread < copy_num_partition; thread++){
            props[thread].thread_index = thread % NUM_THREADS;
            props[thread].final = props[thread].inicio + chunks->partition[thread];
            props[thread].passo = 1;
            start = props[thread].final;
            if (thread != copy_num_partition) // Pegando o inicio da próxima:
                props[thread + 1].inicio = start;
            printf("Thread[%d] - Inicio: %d - Final: %d\n", props[thread].thread_index, props[thread].inicio, props[thread].final);
        }
        for (int k = 0; k < copy_num_partition; k++) {
            pthread_create(&THREADS[k % NUM_THREADS], NULL, loop, &props[k]);
        }
        for (int k = 0; k < copy_num_partition; k++) {
            pthread_join(THREADS[k], NULL);
        }
    } else if (schedule == 3) {
        printf("Como funciona o escalonador guiado.\n");
        Divisions *chunks = schedule_guided(chunk_size, NUM_THREADS, final);
        int copy_num_partition = (int) chunks->num_partition;
        Args props[30] = {inicio, 1, final, 0, f};
        float ondas = ceil((float)copy_num_partition / (float)NUM_THREADS);
        int start;
        // Configurando os argumentos de cada thread:
        for (int thread = 0; thread < copy_num_partition; thread++){
            props[thread].thread_index = thread % NUM_THREADS;
            props[thread].final = props[thread].inicio + chunks->partition[thread];
            props[thread].passo = 1;
            start = props[thread].final;
            if (thread != copy_num_partition) // Pegando o inicio da próxima:
                props[thread + 1].inicio = start;
            printf("Thread[%d] - Inicio: %d - Final: %d\n", props[thread].thread_index, props[thread].inicio, props[thread].final);
            
        }
        for (int k = 0; k < copy_num_partition; k++) {
            pthread_create(&THREADS[k % NUM_THREADS], NULL, loop, &props[k]);
        }
        for (int k = 0; k < copy_num_partition; k++) {
            pthread_join(THREADS[k], NULL);
        }
    } else {
        printf("Escalonador inválido\n");
        printf("Retornando ao main:\n");
        main();
    }
}