#include <iostream>
#include <string>

using namespace std;
struct Node {
    int id;
    int tempo;
    struct Node *next = NULL;
};
class Fila {
    private:
        Node *tail = NULL;
        Node *head = NULL;

    public:
        Fila() { head = tail; }
        void insert(Node *N);
        Node* showTail() {return tail;};
        Node* showHead() {return head;};
        void setHead(Node *N) {head = N;};
        void setTail(Node *N) {tail = N;};
};
class Pilha {
    private:
        Node *top = NULL;
    public:
        void Unloader();
        void setTop(Node *N) {top = N;};
        Node* showTop() {return top;};
};

class Lista {
    private:
        Node *sentinel = NULL;
        Node *processor = NULL;
        Node *escalonador = NULL;
    public:
        Lista() {
            processor = sentinel;
            escalonador = sentinel;
        }

        void proc(int T);
        void Scheduler(Fila *input, Pilha *output);
};

Node* Load(int x, int y);

int main() {
    int K, X, T;
    string process;
    Fila *input = new Fila();
    Lista *work = new Lista();
    Pilha *output = new Pilha();
    Node *novoNo = NULL;
    
    cin >> K;
    while (process != "END") {
        cin >> process;
        if (process == "LOAD") {
            cin >> X >> T;
            novoNo = Load(X, T);
            input->insert(novoNo);

        } else if (process == "UNLD") {
            output->Unloader();

        } else if (process == "PROC") {
            work->Scheduler(input, output);
            work->proc(K);
        }
    }
}

Node* Load(int x, int y) {
    Node *N;
    N = (Node *) malloc(sizeof(Node));
    if (N == NULL) {
        printf("Error na criacao do no\n");
        return NULL;
    }
    N->id = x; //ID
    N->tempo = y; //Tempo
    return N;
}

void Fila::insert(Node *N) {
    if (head == NULL) head = N;
    else head->next = N;
    
    N->next = (tail == NULL) ? NULL : tail;
    tail = N;
}

void Lista::Scheduler(Fila *input, Pilha *output) {
    Node *head, *tail, *top;
    tail = input->showTail();
    head = input->showHead();
    top = output->showTop();
    Node *p = sentinel;

    if (sentinel != NULL && escalonador->tempo <= 0) {
        if (escalonador == sentinel) {
            // Se só tiver um nó
            if (sentinel->next == NULL) {
                sentinel = NULL;
                processor = NULL;
            }
            else sentinel = sentinel->next;
            
            // Encontrando o último nó
            while (p->next != NULL) {
                p = p->next;
            }
            // Joguei o nó na pilha
            escalonador->next = top;
            output->setTop(escalonador);
            
            if (processor == escalonador) processor = p;
            escalonador = p;

        } else {
            Node *proximo = NULL;
            //Se o escalonador não for o último
            if (escalonador->next != NULL) proximo = escalonador->next;
            //Colocando nó na pilha
            escalonador->next = top;
            output->setTop(escalonador);
            
            //Encontrando o nó anterior ao escalonador:
            while (p->next != escalonador) {
                p = p->next;
            }
        
            //Se o próximo é nulo 
            if (proximo == NULL) {
                if (processor == escalonador) processor = p;
                escalonador = p;
                escalonador->next = NULL;
            } else {
                if (processor == escalonador) processor = p;
                escalonador = p;
                escalonador->next = proximo;
            }
        } 
    }

    if (head != NULL) { //Fila não está vazia
        p = tail;
        //Buscando o segundo nó
        if (tail != head) {
            while (p->next != head) {
                p = p->next;
            }
            input->setHead(p);
            //Nó torna circular
            p = p->next;
            head->next = tail;
        } else {
            input->setTail((Node *){NULL});
            tail = input->showTail();
            input->setHead(tail);
        }

        //Tirando o nó
        if (sentinel == NULL) {
            sentinel = p;
            processor = p;
            escalonador = p;
            sentinel->next = NULL;

        } else {
            if (escalonador->next == NULL) {
                escalonador->next = p;
                escalonador = p;
                p->next = NULL;
            } else {
                Node *aux = NULL;
                aux = escalonador->next;
                p->next = aux;
                escalonador->next = p;
                escalonador = p;
            }
        }
    } 
}

void Lista::proc(int K) {
    if (processor != NULL) {
        processor->tempo = (processor->tempo - K) < 0 ? 0 : processor->tempo - K;
        printf("PROC %d %d\n", processor->id, processor->tempo);

        if (processor->next == NULL) processor = sentinel;
        else processor = processor->next;
        
        if (escalonador->next == NULL) escalonador = sentinel;
        else escalonador = escalonador->next; 

    } else {
        printf("PROC -1 -1\n");
    }
}

void Pilha::Unloader() {
    Node *p = NULL;
    
    if (top != NULL) {
        p = top;
        top = p->next;
        printf("UNLD %d\n", p->id);
        free(p); 
    }
}

        // Node *p = NULL;
        // p = sentinel;
        // while (p != NULL) {
        //     printf("%d ", p->id);
        //     p = p->next;
        // }
        // printf("\nESC: %d\n", escalonador->id);