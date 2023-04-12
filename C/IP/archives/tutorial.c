#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// - Modos de abertura de arquivo (w, r, wb, rb, a+)
// - Abrir txt (fprintf e fscanf) escrita e leitura
// - Abrir arquivo binário (fwrite e fread) escrita e leitura
// - Funções auxilíares (fseek, ftell, rewind)
// - %[^\0] vai até o final do arquivo
// - Usar sscanf()!!!!

typedef struct {
    int x, y;
    float value;
} Cell;

int main(void) {
// Declarando variáveis
    int i, size; 
    char buff[255], buffer[100];
    char str[] = "Para ser escrito com fwrite\n";
    FILE *file = NULL;

// - Abrir arquivos não binários

// Abrir com "w" - apaga TUDO que tinha no arquivo anterior
    file = fopen("arquivo.txt", "w");
    if (file == NULL) {
        printf("Deu ruim, linha 25\n");
        exit(1);
    }
    fprintf(file, "Foi escrito com fprintf\n");
    fprintf(file, "%s", str);

// Sempre, SEMPRE dar fclose
    fclose(file);

// Abrir com "r" - Mantém os dados do arquivo, apenas leitura
    file = fopen("arquivo.txt", "r");
    if (file == NULL) {
        printf("Deu ruim, linha 37\n");
        exit(1);
    }
    fscanf(file, " %[^\0]", buff); //Até o final
    printf("1: %s\n", buff);

    fscanf(file, " %[^\0]", buff); // Até o final
    printf("2: %s\n", buff);
    
    fclose(file);

// Abrir com "a+" - Mantém os dados, leitura e escrita
    file = fopen("arquivo.txt", "a+");
    if (file == NULL) {
        printf("Deu ruim, linha 48");
        exit(1);
    }
    fscanf(file, " %s", buff);
    fprintf(file, "pqp não funciona de verdade\n");
    // fflush(file);
    printf("3: %s\n", buff);

    fclose(file);

// - Abrir arquivos binários

// Preparando um vetor do tipo struct, pra por no binário
    Cell celulas[5];
    for (i = 0; i < 5; i++) {
        celulas[i].x = i;
        celulas[i].y = 5-i;
        celulas[i].value = (celulas[i].x * celulas[i].y);
    }

    // printf("\n----------------------\n");
    // printf("Mostrando o que foi atribuído:\n");
    // printf("----------------------\n");

    // for (i = 0; i < 5; i++) {
    //     printf("Cell %d\n", i);
    //     printf("Cell x: %d\n", celulas[i].x);
    //     printf("Cell y: %d\n", celulas[i].y);
    //     printf("Cell value: %f\n\n", celulas[i].value);
    // }

// Agora escrevendo no arquivo novo "wb":

    file = fopen("binario.bin", "wb");
    if (file == NULL) {
        printf("Deu ruim, linha 85\n");
        exit(1);
    }
    fwrite(celulas, sizeof(Cell), 5, file);

    fclose(file);

// Usando "rb", leitura binária:
    file = fopen("binario.bin", "rb");
// Mover o cursor para o final, pra poder contabilizar
    fseek(file, 0, SEEK_END);
//Obtendo a quantidade de bytes do começo até o cursor
    size = ftell(file);
    size = size / sizeof(Cell);
// Movendo o cursor de volta pra o começo, pra ler
    rewind(file);
    // fseek(file, 0, SEEK_SET);

// - Alocando um vetor na memória pra pegar as parada
    Cell *receberCelulas = NULL;
    receberCelulas = (Cell *) malloc(size * sizeof(Cell));
    if (receberCelulas == NULL) {
        printf("Deu ruim, linha 107");
        exit(1);
    }

// Alocou agora vamos preencher com o binário
    fread(receberCelulas, sizeof(Cell), size, file);

// Agora ver se tudo correu bem com a leitura
    // printf("----------------------\n");
    // for (i = 0; i < size; i++) {
    //     printf("Cell %d\n", i);
    //     printf("Cell x: %d\n", receberCelulas[i].x);
    //     printf("Cell y: %d\n", receberCelulas[i].y);
    //     printf("Cell value: %f\n\n", receberCelulas[i].value);
    // }
    // printf("----------------------\n");

    fclose(file);
}

/*
  printf("----------------------\n");
    printf("Método alternativo de leitura\n");
    printf("----------------------\n");

    fp = fopen("arquivo.bin", "rb");
    rewind(fp); // Voltando o ponteiro para o começo do arquivo (só to fazendo isso pq a gente tava lendo nele antes, mas quando o arquivo eh aberto o ponteiro já ta no começo!)

    // fread retorna a quantidade de bytes lidos... então enquanto tentarmos ler e ele retornar valors diferentes de 0 (leu com sucesso), tamo safe!
    cell celulazinha;
    cell *celulasRecebidas2 = NULL; // boa pratica
    int quantidade = 0;
    while(fread(&celulazinha, sizeof(cell), 1, fp) != 0) {
        // O bom e velho realloc
        celulasRecebidas2 = (cell *) realloc(celulasRecebidas2, (quantidade + 1) * sizeof(cell));

        if(celulasRecebidas2 == NULL){
        printf("deu ruim celulasRecebidas2 na alloc %d", (quantidade + 1));
        exit(-1);
        }
        // Atribui a celula lida
        celulasRecebidas2[quantidade] = celulazinha;
        // Incrementa quantidade
        quantidade++;
    }

    // Boh debugar, esse jeito é mto feio
    for(i = 0; i < quantidade; i++) {
        printf("Cell %d\n", i);
        printf("Cell x: %d\n", celulasRecebidas2[i].x);
        printf("Cell y: %d\n", celulasRecebidas2[i].y);
        printf("Cell value: %f\n\n", celulasRecebidas2[i].value);
    }

    printf("----------------------\n");

    // Fecha o arquivaum
    fclose(fp); 
*/