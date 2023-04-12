#include <iostream>

using namespace std;
//FALTA VER ESSE DAQUI!
typedef struct Node{
    int val;
    struct Node *next;

    Node* insert(Node *tail, int y) {
        Node *N;
        N = (Node *) malloc(sizeof(Node));
        if (N == NULL) {
            cout << "Error";
            return NULL;
        }
        N->val = y;
        N->next = (tail->next) == NULL ? tail : tail->next;
        tail->next = N;

        return tail;
    }

    void list_delete(Node *head) {
        Node *p = NULL;
        p = head->next;
        if (head->next->next != NULL) {
            while (p->next != NULL) {
                p = p->next;
                head = head->next;
                cout << head->next << endl;
            }
            head->next = NULL;
            int v = p->val;
            cout << endl << "Saiu " << v << endl;
            free(p); 
        }
    }

    void imprimir(Node *tail) {
        Node *p = NULL;
        p = tail;
        for (int i = 0; i < 20; i++) {
            if (tail->val != 0) {    
                cout << tail->val << endl;
            }
            tail = tail->next;
        }
    }

};

int main() {
    Node *tail = NULL, *head = NULL;

    head = (Node *) malloc(sizeof(Node));
    if (head == NULL) printf("Chorou1");

    tail = (Node *) malloc(sizeof(Node));
    if (tail == NULL) printf("Chorou2");

    tail->next = NULL;
    head->next = tail;
    tail->val = 0;
    int escolha, valor;

    do {
    printf("\n0. Sair\n1. Enfileirar\n2. Desenfileirar\n");
        scanf("%d", &escolha);
        if (escolha == 1) {
            printf("Digite o valor: \n");
            scanf("%d", &valor);
            tail->insert(tail, valor);

        } else if (escolha == 2) {
            head->list_delete(head);
            
        } else {
            if (escolha == 0) {
                printf("Saindo\n");
            } else {
                printf("Escolha invalida");
                scanf("%d", &escolha);
            }
        }
    } while (escolha != 0);
    
    free(tail);
}