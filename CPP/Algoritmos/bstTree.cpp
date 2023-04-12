#include <iostream>

using namespace std;

struct Node {
    Node *left = NULL;
    int value = 0;
    Node *right = NULL;
};

void visit(Node *root, int &count) {
    if (!count) {
        cout << root->value;
        count += 1;
    } else {
        cout << " " << root->value;
    }
}

void posOrder(Node *root, int &count) {
    if (root == NULL) {
        return ;
    }    
    posOrder(root->left, count);
    posOrder(root->right, count);
    visit(root, count);
}

int height(Node *root) {
    int HL = 0, HR = 0;
    if (root == NULL) {
        return 0;   
    }    
    HL = height(root->left);
    HR = height(root->right);
    return 1+max(HL, HR);
}

int balance(Node *root) {
    if (root == NULL){
        return 1;
    }
    if (abs(height(root->left) - height(root->right)) <= 1 && balance(root->left) && balance(root->right)) {
        return 1;
    }
    return 0;
}

Node *insert(Node *root, int v) {
    Node *N = NULL;
    if (root == NULL) {
        N = new Node[1];
        N->value = v;
        return N;
    }
    if (v < root->value) {
        root->left = insert(root->left, v);
        return root;

    } else {
        root->right = insert(root->right, v);
        return root;
    }
} 

Node *left(Node *root) {
    Node *R, *RL;
    R = root->right;
    RL = R->left;
    R->left = root;
    root->right = RL;
    return R;
}

Node *right(Node *root) {
    Node *R, *RR;
    R = root->left;
    RR = R->right;
    R->right = root;
    root->left = RR;
    return R;
}

Node *etapa(Node *root, int *L) {
    if (root == NULL) {
        return root;
    } 

    if (root->right != NULL) {
        root = left(root);
        *L = *L + 1;
        root = etapa(root, L);
    } else {
        root->left = etapa(root->left, L);
    }
    return root;
}

Node *etapa1(Node *root, int *L, int *R) {
    if (root == NULL) {
        return root;
    }
    int x;
    cin >> x;
    if (root->right == NULL) {
        while (root->value != x) {
            root = right(root);
            *R = *R + 1;
        }
    } else {
        while (root->value != x) {
            root = left(root);
            *L = *L + 1;
        }
    }
    root->left = etapa1(root->left, L, R);
    root->right = etapa1(root->right, L, R);
    return root;
}

int main() {
    int N = 0, x = 0;
    bool fragoso = false;
    
    while (scanf("%d", &N) != EOF) {
        if (fragoso) 
            cout << endl;
        Node *source = NULL;
        bool verif = false;
        int L = 0, R = 0, count = 0;

        for (int i = 0; i < N; i++) {
            cin >> x;
            source = insert(source, x);
        }
    //Etapa 1
        source = etapa(source, &L);

    //Etapa 2
        source = etapa1(source, &L, &R);

        cout << L << " " << R << endl;
        posOrder(source, count);

        if (balance(source)) {
            verif = true;
        }
        if (verif) {
            cout << endl << "true" << endl;
        } else {
            cout << endl << "false" << endl;
        }
        fragoso = true;
    }
    cout << endl;

    return 0;
}