#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    float pReal, pImag;
} Complexo;

void empilhe(Complexo **cPilha, Complexo umC, int *tPilha) {
    (*cPilha) = (Complexo*) realloc(*cPilha, (*tPilha+1) * sizeof(Complexo));
    if (*cPilha == NULL) {
        printf("Nao alocou\n");
        exit(1);
    }
    (*cPilha)[*tPilha] = umC;
    (*tPilha) += 1;
}

Complexo desempilhe(Complexo **cPilha, int *tPilha) {
    if (*tPilha > 0) {
        Complexo complexo = (*cPilha)[*tPilha-1];
        (*cPilha) = (Complexo*) realloc(*cPilha, (*tPilha-1) * sizeof(Complexo));
        if (*cPilha == NULL && *tPilha != 1) {
            printf("Nao alocou\n");
            exit(1);
        }
        (*tPilha) -= 1;
        return complexo;
    }
}

Complexo topo(Complexo *cPilha, int tPilha) {
    if (!pilhaVazia(cPilha, tPilha)) {
        return cPilha[tPilha-1];
    }
}

int pilhaVazia(Complexo *cPilha, int tPilha) {
    if (cPilha == NULL || tPilha == 0) return 1;
    else return 0;
}

void salvePilha(Complexo *cPilha, int tPilha) {
    FILE *file = NULL;

    file = fopen("pilha.bin", "wb");
    if (file == NULL) {
        printf("Erro ao abrir o arquivo");
        free(cPilha);
        exit(1);
    }
    fwrite(&tPilha, sizeof(int), 1, file);
    fwrite(cPilha, sizeof(Complexo), tPilha, file);
    
    fclose(file);
}

Complexo *recuperePilha(int *tPilha) {
    Complexo *pilha;
    FILE *file = NULL;

    file = fopen("pilha.bin", "rb");
    if (file == NULL) {
        printf("Falha ao abrir o arquivo");
        exit(1);
    }
    
    fread(tPilha, sizeof(int), 1, file);

    pilha = (Complexo *) malloc(*tPilha * sizeof(Complexo));
    if (pilha == NULL) {
        printf("Nao conseguiu alocar\n");
        fclose(file);
        exit(1);
    }
    
    fread(pilha, sizeof(Complexo), *tPilha, file);

    fclose(file);

    return pilha;
}

int main(void) {
    Complexo *cPilha = NULL, umC;
    int tPilha = 0;

    umC.pImag = 0;
    umC.pReal = 0;

    if (pilhaVazia(cPilha, tPilha)) {
        printf("Tudo certo com a pilha inicial\n");

        umC.pImag = 2.5;
        umC.pReal = 3.5;
        empilhe(&cPilha, umC, &tPilha);

        umC.pImag = -1.5;
        umC.pReal = 2.0;
        empilhe(&cPilha, umC, &tPilha);

        umC = topo(cPilha, tPilha);
        if (umC.pImag == -1.5 && umC.pReal == 2.0) {
            salvePilha(cPilha, tPilha);

            desempilhe(&cPilha, &tPilha);
            desempilhe(&cPilha, &tPilha);

            cPilha = recuperePilha(&tPilha);
            //Verificando
            umC = desempilhe(&cPilha, &tPilha);
            
            if (umC.pImag == -1.5 && umC.pReal == 2.0) {
                umC = desempilhe(&cPilha, &tPilha);
                
                if (umC.pImag == 2.5 && umC.pReal == 3.5) {
                    printf("Recuperado corretamente");
                }
            }
        }
    }
    free(cPilha);
}