#include <iostream>

using namespace std;

typedef struct Node {
    int val;
    struct Node *next;

    Node* insert(Node *cur, int y) {
        Node *N;
        N = (Node *) malloc(sizeof(Node));
        if (N == NULL) {
            cout << "Error";
            return NULL;
        }
        N->val = y;
        N->next = cur->next;
        cur->next = N;
        return cur;
    }

    Node* acess(Node *head, int pos) {
        Node *cur;
        cur = head;

        for (int i = 0; i < pos && cur->next != NULL; i++) {
            cur = cur->next;
        }
        
        return cur;
    };

    Node* list_delete(Node *cur) {
        Node *p = NULL;
        if (cur->next != NULL) {
            p = cur->next;
            cur->next = p->next;
            int v = p->val;
            cout << "Saiu " << v << endl;
            free(p); 
        }
        return cur;
    }

    Node* procurar(Node *head, int x) {
        Node *cur = head;
        int i;
        for(i = 0; cur->next != NULL && cur->next->val != x; i++) {
            cur = cur->next;
        }
        cout << "Estou na posicao: " << i << endl;
        return cur;
    }

    void imprimir(Node *head) {
        Node *cur = head;
        cur = cur->next;

        while (cur->next != NULL) {
            cur = cur->next;
            cout << "Valor: " << cur->val << endl;
        }
    }
};


int main() {
    Node *atual = NULL, *cabeca = NULL;
    
    cabeca = (Node *) malloc(sizeof(Node));
    if (cabeca == NULL) printf("Chorou1");

    atual = (Node *) malloc(sizeof(Node));
    if (atual == NULL) printf("Chorou2");

    cabeca->next = atual;
    atual->next = NULL;

    int escolha, valor;

    do {
    printf("\n0. Sair\n1. Inserir\n2. Acessar\n3. Deletar\n4. Procurar\n5. Mostrar\n");
        scanf("%d", &escolha);
        if (escolha == 1) {
            printf("Digite o valor: \n");
            scanf("%d", &valor);
            atual->insert(atual, valor);

        } else if (escolha == 2) {
            printf("Digite a posicao: \n");
            scanf("%d", &valor);
            atual->acess(cabeca, valor);

        } else if (escolha == 3) {
            printf("O no atual sera destruido\n");
            atual->list_delete(atual);

        } else if (escolha == 4) {
            printf("Digite o numero: \n");
            scanf("%d", &valor);
            atual->procurar(cabeca, valor);

        } else if (escolha == 5) {
            cabeca->imprimir(cabeca);

        } else {
            if (escolha == 0) {
                printf("Saindo\n");
            } else {
                printf("Escolha invalida");
                scanf("%d", &escolha);
            }
        }
    } while (escolha != 0);
    
    free(cabeca);
    free(atual);
}   