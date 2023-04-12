#include <iostream>

using namespace std;

class Node {
    private:
        int value;
        Node *prev = NULL;

    public:
        //Pode colocar tudo num construtor:
        void setVal(int valor) {value = valor;};
        int getVal() {return value;};

        void setPrev(Node *anterior);
        Node *getPrev();

        //Construtor:
        // Node (int inicial, Node *anterior) {
        //     value = inicial;
        //     prev = anterior;
        // }  
};

void Node::setPrev(Node *anterior) {
    prev = anterior;
}
Node* Node::getPrev() {
    return prev;
}

int main() {
    Node node1, node2;
    node1.setVal(1);
    node2.setVal(2);

    node2.setPrev(&node1);

    cout << node1.getPrev() << endl;
    cout << node1.getVal() << endl << "--------" << endl;
    
    cout << node2.getPrev()->getVal() << endl;
    cout << node2.getVal() << endl;
    
}