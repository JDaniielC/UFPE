#include <stdio.h>
#include <string.h>
#include <stdlib.h>

//jdsc2

typedef struct {    
    char rementente[50];
    char assunto[50];
    char mensagem[200];
} Email;

typedef struct {
    char nome[50];
    char senha[50];
    int qtdRecebidos, qtdEnviados;
    Email *recebidos;
    Email *enviados;
} Usuario;

void cadastrarUsuario(Usuario **user, int *size, char *nome, char *senha) {
    int i;
    FILE *file = NULL;
    //Evitando que crie outro com mesmo nome.
    for (i = 0; i < *size; i++) {
        if (strcmp((*user)[i].nome, nome) == 0) {
            printf("Este usuario ja existe\n");
            return;
        }
    }
    //Cria um novo usuário com nome e senha
    (*user) = (Usuario *) realloc(*user, (*size + 1) * sizeof(Usuario));
    strcpy((*user)[*size].nome, nome);
    strcpy((*user)[*size].senha, senha);
    //Importante para a função enviarEmail:
    (*user)[*size].qtdRecebidos = 0;
    (*user)[*size].qtdEnviados = 0;

    *size += 1;
    //Insere no arquivo de usuários (users.bin)
    file = fopen("users.bin", "wb");
    if (file == NULL) {
        printf("Nao foi possivel abrir o arquivo");
        return;
    }
    fwrite(*user, sizeof(Usuario), *size, file);

    fclose(file);
}

void login(char *nome, char *senha) {
    int menu = -1, i, size, frag = 0, m;
    char nomeRemetente[50];
    FILE *file = NULL;
    Usuario *user = NULL;
    //Lê o arquivo de usuários
    file = fopen("users.bin", "rb");
    if (file == NULL) {
        printf("Nao foi possivel abrir o arquivo");
        return;
    }
    
    fseek(file, 0, SEEK_END);
    size = ftell(file);
    rewind(file);

    user = (Usuario *) malloc(size);
    if (user == NULL) {
        printf("Nao conseguiu alocar\n");
        fclose(file);
        return;
    }

    size = size / sizeof(Usuario);
    fread(user, sizeof(Usuario), size, file);


    fclose(file);
    //Procura pelo usuário com nome
    for (i = 0; i < size; i++) {
        if (strcmp(user[i].nome, nome) == 0) {
            frag = 1;
            m = i;
        }
    }
    //Solicitando nome novamente
    while (frag == 0) {
        printf("Este usuario invalidos, tente novamente.\n");
        printf("Insira nome do usuario:\n");
        scanf(" %s", nome);
        for (i = 0; i < size; i++) {
            if (strcmp(user[i].nome, nome) == 0) {
                frag = 1;
                m = i;
            }
        }
    }
    frag = 0;

    //Verifica a senha
    for (i = 0; i < size; i++) {
        if (strcmp(user[m].senha, senha) == 0) {
            frag = 1;
        }
    }
    //Solicitando senha novamente
    while (frag == 0) {
        printf("Senha incorreta\n");
        printf("Digite sua senha novamente:\n");
        scanf(" %s", senha);
        if (strcmp(user[m].senha, senha) == 0) {
            frag = 1;
        }  
    }

    printf("Login bem sucedido\n");
    //Opções de ver e-mails recebidos, ver e-mails enviados e enviar e-mail.
    while (menu != 0) {
        printf("- 1. Enviar\n- 2. Ver recebidos\n- 3. Ver enviados\n- 0. Sair\n");
        scanf("%d", &menu);
        switch (menu) {
            case 1:
                enviarEmail(user[m].nome, user[m]);
                break;
            case 2:
                verRecebidos(user[m]);
                break;
            case 3:
                verEnviados(user[m]);
                break;
            default: 
                printf("Escolha incomum\n");
                break;
        }
    }

    free(user);
}

void enviarEmail(char *nomeRemetente) {
    FILE *file = NULL;
    Usuario *users = NULL;
    char nome[50], assunto[50], mensagem[200];
    int size, i, frag, m, n, qtd;

    file = fopen("users.bin", "rb");
    if (file == NULL) {
        printf("Nao foi possivel abrir o arquivo");
        return;
    }
    
    fseek(file, 0, SEEK_END);
    size = ftell(file);
    rewind(file);

    users = (Usuario *) malloc(size);
    if (users == NULL) {
        printf("Nao conseguiu alocar\n");
        fclose(file);
        exit(1);
    }

    size = size / sizeof(Usuario);
    fread(users, sizeof(Usuario), size, file);

    fclose(file);

    //Solicita o nome do destinatário
    printf("Nome do destinatario\n");
    scanf(" %s", nome);
    //Procura o destinatário no arquivo
    for (i = 0; i < size; i++) {
        if (strcmp(users[i].nome, nome) == 0) {
            frag = 1;
            m = i;
        }
    }

    while (frag == 0) {
        printf("Nao foi possivel encontrar este usuario\n");
        printf("Insira nome do destinatario:\n");
        scanf(" %s", nome);
        for (i = 0; i < size; i++) {
            if (strcmp(users[i].nome, nome) == 0) {
                frag = 1;
                m = i;
            }
        }
    }
    //Destinatario encontrado.
    qtd = users[m].qtdRecebidos;
    if (qtd == 0) {
        //Pra não dar erro no realloc
        users[m].recebidos = NULL;
    }
    //Alocando dinâmicamente os emails.
    users[m].recebidos = (Email *) realloc(users[m].recebidos, (qtd+1)*sizeof(Email));
    strcpy(users[m].recebidos[qtd].rementente, nomeRemetente);
    //Solicita assunto
    printf("Assunto:\n");
    scanf(" %49[^\n]", assunto);
    strcpy(users[m].recebidos[qtd].assunto, assunto);
    //Solicita mensagem a ser enviada
    printf("Escreva a mensagem\n");
    scanf(" %199[^\n]", mensagem);
    strcpy(users[m].recebidos[qtd].mensagem, mensagem);

    printf("-----------------------------");
    printf("%d. %s\n", qtd, users[m].recebidos[qtd].rementente);
    printf("%d. %s\n", qtd, users[m].recebidos[qtd].assunto);
    printf("%d. %s\n", qtd, users[m].recebidos[qtd].mensagem);

    //Procura o remetente no arquivo e guarda o e-mail no vetor de e-mails enviados
    for (i = 0; i < size; i++) {
        if (strcmp(users[i].nome, nomeRemetente) == 0) {
            n = i;
        }
    }
    //Remetente encontrado.
    qtd = users[n].qtdEnviados;
    if (qtd == 0) {
        //Pra não dar erro no realloc
        users[n].enviados = NULL;
    }
    //Alocando dinâmicamente os emails.
    users[n].enviados = (Email *) realloc(users[n].enviados, (qtd+1)*sizeof(Email));
    strcpy(users[n].enviados[qtd].rementente, nome);
    strcpy(users[n].enviados[qtd].assunto, assunto);
    strcpy(users[n].enviados[qtd].mensagem, mensagem);

    printf("-----------------------------");
    printf("%d. %s\n", qtd, users[m].enviados[qtd].rementente);
    printf("%d. %s\n", qtd, users[m].enviados[qtd].assunto);
    printf("%d. %s\n", qtd, users[m].enviados[qtd].mensagem);
    printf("-----------------------------");

    users[m].qtdRecebidos += 1;
    users[n].qtdEnviados += 1;
    //Atualiza o arquivo (users.bin)
    file = fopen("users.bin", "wb");
    if (file == NULL) {
        printf("Nao foi possivel abrir o arquivo");
        return;
    }
    fwrite(users, sizeof(Usuario), size, file);

    fclose(file);

    //Free!!!!!
    for (i = 0; i < qtd+1; i++) {
        free(users[m].recebidos);
        free(users[n].enviados);
    }
    free(users);
}

void verRecebidos(Usuario usuario) {
    int i;
    for (i = 0; i < usuario.qtdRecebidos; i++) {
        printf("De: %s\n", usuario.recebidos[i].rementente);
        printf("- %s\n", usuario.recebidos[i].assunto);
        printf("%s\n", usuario.recebidos[i].mensagem);
        printf("----------------\n");
    }
}

void verEnviados(Usuario usuario) {
    int i;
    for (i = 0; i < usuario.qtdEnviados; i++) {
        printf("Para: %s\n", usuario.enviados[i].rementente);
        printf("- %s\n", usuario.enviados[i].assunto);
        printf("%s\n", usuario.enviados[i].mensagem);
        printf("----------------\n");
    }
}

void main() {
    int menu, size = 0;
    char nome[50], senha[50];
    Usuario *user = NULL;
    FILE *file = NULL;
    //Abrindo o arquivo pra mandar os dados antes de cadastrar!
    file = fopen("users.bin", "rb");
    if (file != NULL) {
        fseek(file, 0, SEEK_END);
        size = ftell(file);
        rewind(file);

        user = (Usuario *) malloc(size);
        if (user == NULL) {
            printf("Nao conseguiu alocar\n");
            fclose(file);
            exit(1);
        }
        size = size / sizeof(Usuario);
        fread(user, sizeof(Usuario), size, file);
        
    } else {
        printf("Arquivo nao encontrado\n");
    }

    fclose(file);

    while (menu != 0) {
        printf("- 1. Cadastro\n- 2. Login\n- 0. Sair\n");
        scanf("%d", &menu);
        switch (menu) {
            case 1:
                printf("Cadastrar usuario, digite o nome\n");
                scanf(" %s", nome);
                printf("Digite a senha\n");
                scanf(" %s", senha);
                cadastrarUsuario(&user, &size, nome, senha);
                break;
            case 2:
                printf("Cadastrar usuario, digite o nome\n");
                scanf(" %s", nome);
                printf("Digite a senha\n");
                scanf(" %s", senha);
                login(nome, senha);
                break;
            default:
                printf("Escolha incomum.\n");
                break;
        }
    }

    free(user);
}