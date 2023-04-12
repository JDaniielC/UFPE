#include <stdio.h>
#include <string.h>
#include <stdlib.h>

//Código mais fofinho que vocês vão ver na vida, bons estudos!!! - JDaniel

void atividade1() {
    FILE *file = NULL;
    char frase[100], mostrar[100];

    file = fopen("arquivo.txt", "a+");
    if (file == NULL) {
        printf("Deu ruim na primeira atividade\n");
        return;
    }

    printf("Digite a frase, nao esquece do 0\n");
    scanf(" %[^0]", frase);
    fprintf(file, "%s", frase);

    fclose(file);

    file = fopen("arquivo.txt", "a+");
    if (file == NULL) {
        printf("Deu ruim na primeira atividade\n");
        return;
    }

    printf("O que está no arquivo 1:\n");
    while (fgets(mostrar, 100, file) != NULL) {
        printf("%s", mostrar);
    }
    
    printf("\n");

    fclose(file);
}

void atividade2() {
    FILE *file = NULL;
    int lines = 0;
    char frase[100], fileName[100];

    printf("Escreva o nome do arquivo.txt");
    scanf(" %[^\n]", fileName);

    file = fopen(fileName, "r");
    if (file == NULL) {
        printf("Nao abriu o arquivo\n");
        return;
    }

    while(!feof(file)) {
        fscanf(file, " %[^\n]", frase);
        lines++;
    }
    printf("%d", lines);

    fclose(file);
}

void copyArchive(char name[]) {
    FILE *file = NULL;
    FILE *alvo = NULL;
    char arquivo[500];
    printf("%s\n", name);
    file = fopen(name, "r");
    if (file == NULL) {
        printf("Nao conseguiu entrar no arquivo\n");
        return;
    }

    alvo = fopen("novoArquivo.txt", "a+");
    if (alvo == NULL) {
        printf("Nao entrou no novo arquivo\n");
        return;
    }

    fscanf(file, " %[^\0]", arquivo);
    fprintf(alvo, "%s", arquivo);

    fclose(alvo);
    fclose(file);
}

void atividade3() {
    FILE *file = NULL;
    char fileName[100], fileName2[100], content[500];

    printf("Digite o nome do primeiro arquivo\n");
    scanf(" %s", fileName);
    printf("Digite o nome do segundo arquivo\n");
    scanf(" %s", fileName2);

    copyArchive(fileName);
    copyArchive(fileName2);

    // file = fopen("novoArquivo.txt", "r");
    // if (file == NULL) {
    //     printf("Nao conseguiu abrir o arquivo\n");
    //     return;
    // }

    // fscanf(file, " %[^\0]", content);
    // printf("Dentro do novo arquivo: %s\n", content);

    fclose(file);
}

void atividade4() {
    FILE *file = NULL;
    char fileName[30], nome[50], auxN[50];
    int nota = 0, aux;
    
    printf("Digite o nome do arquivo.txt\n");
    scanf(" %s", fileName);
    file = fopen(fileName, "r");
    if (file == NULL) {
        printf("Nao foi possivel abrir o arquivo\n");
        return;
    }

    // while (fgets(linha, lin, fp1)!=feof(fp1)){
    //     sscanf(linha, "NOME: %s NOTA: %d", aluno[i].nome, &aluno[i].nota);
    //     i++;
    // } Jeito do professor!

    while (!feof(file)) {
        fscanf(file, " %s", auxN);
        fscanf(file, "%d", &aux);
        // fscanf(arq, "NOME: %s NOTA: %f\n", nome, &nota);

        if (aux > nota) {
            nota = aux; 
            strcpy(nome, auxN);
        }
    }

    printf("Nome: %s, Nota: %d\n", nome, nota);

    fclose(file);
}

typedef struct {
    char nome[50];
    char telefone[20];
    int dia;
    int mes;
} Agenda;

void inserir(Agenda **contato, int qtd) {
    char nome[50], telefone[20];
    int dia, mes;
    (*contato) = (Agenda *) realloc(*contato, (qtd+1) * sizeof(Agenda));
    if ((*contato) == NULL) {
        printf("------------------\nDeu ruim em alocar\n------------------\n");
        return;
    }
    printf("Digite o Nome:\n");
    scanf("%s", nome);
    strcpy((*contato)[qtd].nome, nome);

    printf("Digite o telefone:\n");
    scanf("%s", telefone);
    strcpy((*contato)[qtd].telefone, telefone);

    printf("Digite o dia e mes\n");
    scanf("%d %d", &dia, &mes);
    (*contato)[qtd].dia = dia;
    (*contato)[qtd].mes = mes;
}

int pesquisar(Agenda *contato, int qtd, char nome[]) {
    int i;
    for (i = 0; i < qtd; i++) {
        if (strcmp(nome, contato[i].nome) == 0) {
            return i;
        }
    }
    return;
}

void remover(Agenda *contato, int qtd) {
    char nome[50];
    int num;

    printf("Digite o nome do contato que quer excluir:");
    scanf(" %s", nome);
    num = pesquisar(contato, qtd, nome);
    if (!num) {
        printf("Nao ha esse contato na agenda\n");
        return;
    }

    contato[num] = contato[qtd];
    contato = (Agenda *) realloc(contato, qtd * sizeof(Agenda));
}

void listar(Agenda *contato, int qtd) {
    int i;
    printf("------------------------------------\n");
    for (i = 0; i < qtd; i++) {
        printf("Nome: %s\n", contato[i].nome);
        printf("Telefone: %s\n", contato[i].telefone);
        printf("Aniversario: %d/%d\n", contato[i].dia, contato[i].mes);
        printf("------------------------------------\n");
    }
}

void imprimir(Agenda *contato, int qtd) {
    int i;
    for (i = 0; i < qtd; i++) {
        if (contato[i].mes == 8) {
            printf("Nome do aniversariante: %s\n", contato[i].nome);
        }
    }
}

void atividade5() {
    int escolha, qtd = 0;
    Agenda *contatos = NULL;
    FILE *file = NULL;

    file = fopen("contatos.bin", "rb");
    if (file == NULL) {
        printf("Erro ao abrir arquivo");
        return;
    }

    fseek(file, 0, SEEK_END);
    qtd = ftell(file);
    qtd = qtd / sizeof(Agenda); //
    rewind(file);

    contatos = (Agenda *) malloc(qtd * sizeof(Agenda));
    if (contatos == NULL) {
        printf("Nao alocou o arquivo.bin\n");
        return;
    }

    fread(contatos, sizeof(Agenda), qtd, file);
    fclose(file);

    printf("Agenda\n");
    printf("Digite 0 para sair\n1 Inserir contato\n2 Remover contato\n3 Listar contatos\n4 Imprimir aniversariante\n");

    scanf("%d", &escolha);
    while (escolha != 0) {
        switch (escolha)
        {
        case 1:
            inserir(&contatos, qtd);
            qtd++;
            break;
        case 2:
            remover(contatos, qtd);
            qtd--;
            break;
        case 3:
            listar(contatos, qtd);
            break;
        case 4:
            imprimir(contatos, qtd);
            break;
        default:
            printf("Numero invalido\n");
            break;
        }
        printf("Digite 0 para sair\n1 Inserir contato\n2 Remover contato\n3 Listar contatos\n4 Imprimir aniversariante\n");
        scanf("%d", &escolha);
    }
    
    file = fopen("contatos.bin", "wb");
    if (file == NULL) {
        printf("Erro ao abrir arquivo");
        return;
    }
    fwrite(contatos, sizeof(Agenda), qtd, file);

    fclose(file);
    free(contatos);
}

int main(void) {
    int escolha, frag = 0;
    printf("Escolhe um numero\n");
    scanf("%d", &escolha);
    while (escolha != 0) {
        switch (escolha)
        {
        case 1:
            atividade1();
            frag = 1;
            break;
        case 2:
            atividade2();     
            frag = 1;   
            break;
        case 3:
            atividade3();
            frag = 1;
            break;
        case 4:
            atividade4();
            frag = 1;
            break;
        case 5:
            atividade5();
            frag = 1;
            break;
        default:
            if (frag == 0) {
                printf("Numero invalido\n");
                frag = 1;
            }
            break;
        }
        printf("Escolhe um numero\n");
        scanf("%d", &escolha);
    }
    return 0;
}
