#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct{
    double x, fx;
}Ponto;


void validaIntervalo(double *a, double *b){
    double aa, bb;

    do{
        printf("Insira os valores A e B dos limites de integracao: (a < b): \n");
        scanf("%lf %lf", &aa, &bb);

        (*a) = aa;
        (*b) = bb;

    }while((*a) > (*b));

}

void validaPrecisao(double *p){
    double pp;

    do{
        printf("Insira a precisao p que voce deseja(Obs: 0 < p < 1):\n");
        scanf("%lf", &pp);

        (*p) = pp;

    }while((*p) <= 0 || (*p) >= 1);

}

double f(double x){
    return x * x + sin(x);
}

Ponto *gerarVetorPontos(double a, double b, double largura){
    int qtdPontos, i;
    Ponto *vetor = NULL;

    qtdPontos = ceil((b - a) / largura);

    vetor = (Ponto *) malloc(qtdPontos * sizeof(Ponto));
    if(vetor == NULL){
        printf("Erro ao alocar\n");
        exit(-1);
    }

    vetor[0].x = a;
    vetor[0].fx = f(a);

    for(i = 1; i < qtdPontos; i++){
        vetor[i].x = vetor[i - 1].x + largura;
        vetor[i].fx = f(vetor[i].x);
    }

    return vetor;
}

double areaRetangulo(double x, double largura){
    double fx1 = f(x);
    double fx2 = f(x + largura);

    double altura = (fx1 + fx2) / 2.0;

    return altura * largura;
}


Ponto *integral(double a, double b, double p, double *larguraFinal){
    Ponto *vetorPontos = NULL;
    double larguraAtual = p;
    double areaAtual = 0, areaAnterior = 0;
    double precisao = p;
    int i, qtdPontos = 0;

    do{
        areaAnterior = areaAtual;
        qtdPontos = ceil((b - a) / larguraAtual);

        vetorPontos = gerarVetorPontos(a, b, larguraAtual);

        areaAtual = 0;

        for(i = 0; i < qtdPontos; i++){
            areaAtual += areaRetangulo(vetorPontos[i].x, larguraAtual);
        }

        larguraAtual /= 2.0;

    }while(fabs(areaAtual - areaAnterior) > precisao);

    printf("A area total obtida foi %.5lf\n", areaAtual);
    
    free(vetorPontos);

    (*larguraFinal) = (larguraAtual * 2);

    vetorPontos = gerarVetorPontos(a, b, larguraAtual * 2);

    return vetorPontos;

}

void salvaArquivo(double precisao, Ponto *pontos, int qtdPontos){
    FILE *arq = NULL;
    int i;

    arq = fopen("configuracao.bin", "ab");
        if(arq == NULL){
            printf("Erro ao abri o arq\n");
            exit(1);
        }

    fwrite(&precisao, sizeof(double), 1, arq);

    for(i = 0; i < qtdPontos; i++){
        fwrite(&pontos[i].x, sizeof(double), 1, arq);

        fwrite(" ", sizeof(char), 1, arq);

        fwrite(&pontos[i].fx, sizeof(double), 1, arq);

        fwrite("\n", sizeof(char), 1, arq);

    }

    fclose(arq);

}

int main () {
    double a, b, p, larguraFinal;
    Ponto *pontos = NULL;
    int qtdPontosFinal = 0;

    validaIntervalo(&a, &b);

    validaPrecisao(&p);

    pontos = integral(a, b, p, &larguraFinal);

    qtdPontosFinal = ceil((b - a) / larguraFinal);

    if( pontos != NULL){
        salvaArquivo(p, pontos, qtdPontosFinal);
    }

    //leArquivo(qtdPontosFinal); funcao auxiliar para ler os pontos gravados no arquivo
    free(pontos);
    
    return 0;
    
}