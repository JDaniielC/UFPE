#include <stdio.h>

int main() {
    char letra;
    scanf(" %c", &letra); //Dar espaço quando pegar char
    int i, j, elementos, pontos;
    int linhas = letra - 'A' + 1; //+1 pra formar a última linha sem ponto
    int colunas = linhas * 2 - 1; // Linhas contanto com os pontos

    for (i = 1; i <= linhas; i++) {
        elementos = i * 2 - 1; //elementos mudam a cada loop
        pontos = (colunas - elementos)/2; //Quantidade de pontos só de um lado

        for (j = 0; j < pontos; j++) {
            printf(".");
        } //primeira metade de ponto
        
        char letraAtual = 'A' - 1;

        for(j = 0; j < elementos; j++)  {
            if (j <= elementos / 2) {
                letraAtual += 1;
            } else {
                letraAtual -= 1;
            }
            printf("%c", letraAtual);
        } 

        for (j = 0; j < pontos; j++) {
            printf(".");
        } //segunda metade de pontos
        printf("\n");
    }

    return 0;
}


