#include <iostream>
#include <math.h>

using namespace std;
struct Hospital {
    int id = 0;
    int casos = 0;
};

struct Heap {
    Hospital *hospital;
    int size = 0;
    int idTotal = 0;
};

Hospital* bubbleDown(int position, Hospital *H, int size) {
    //Declarações
    int leftC, rightC, midC;
    int leftP, midP, rightP;
    int leftID, midID, rightID;
    Hospital aux;
    //Atribuições
    leftP = 2*position + 1;
    leftC = H[leftP].casos;
    leftID = H[leftP].id;

    midP = position;
    midC = H[position].casos;
    midID = H[position].id;

    rightP = 2*position + 2;
    rightC = H[rightP].casos;
    rightID = H[rightP].id;


    if(leftP < size && leftC > midC) {
        midC = leftC;
    } else {
        if (leftP < size && leftID > midID) {
            midC = leftC;
        }
    }

    if(rightP < size && rightC > midC){
        midC = rightC;
    } else {
        if (rightP < size && rightID > midID) {
            midC = rightC;
        }
    }

    if(midC != position){
        aux = H[midC];
        H[midC] = H[position];
        H[position] = aux;
        return bubbleDown(midC, H, size);
    }
    return H;
}

Hospital* buildHeap(int size, Heap heap, Hospital *hospital) {
    int cont;
    for(cont = floor(size/2) - 1; cont >= 0; cont--){
        bubbleDown(cont, hospital, heap.size);
    }
    return hospital;
}

Hospital* heapExtract(int id, Hospital *hospital, Heap *heap) {
    Hospital aux;
    aux = hospital[id];
    hospital[id] = hospital[(*heap).size-1];
    hospital[(*heap).size-1] = aux;
    (*heap).size--;
    bubbleDown(id, hospital, (*heap).size);
    return hospital;
}

void heapSort(Heap* heap, int id, Hospital* hospital) {
    int contador = heap->size;
    while(contador > 1){
        heapExtract(id, hospital, heap);
        contador--;
    }
}

int verif(int A, int B, int C, int D) {
    if (A > B) {
        return 1;
    } else {
        if (C > D) {
            return 1;
        }
    }
    return 0;
}

Hospital* bubbleUp(int p, Hospital *H) {
    int i = p - 1;
    Hospital aux;

    while(i > 0 && verif(H[i].casos, H[(i-1)/2].casos, H[i].id, H[(i-1)/2].id)){
        aux = H[i];
        H[i] = H[(i-1)/2];
        H[(i-1)/2] = aux;
        i = (i-1)/2;
    }
    return H;
}

Hospital* heapInsert(Hospital *hospital, int casos, int size, int ID) { 
    hospital[size].casos = casos;
    hospital[size].id = ID;
    hospital = bubbleUp(size+1, hospital);
    return hospital;
}

void Del(Hospital *hospital, int id, int *total, Heap *heap) {
    int i;
    i = 0;
    if((*heap).size != 0){
        while(i < (*heap).size){
            if(id == hospital[i].id){
                *total -= hospital[i].casos;
                hospital[i].casos = 0;
                hospital = heapExtract(i, hospital, heap);
                hospital = (Hospital*) realloc(hospital, (*heap).size*sizeof(Hospital));
                i = (*heap).size;    
            }
            i++;
        }
        cout << hospital[0].id << " " << hospital[0].casos << endl;
    }
    else{
        cout << "-1 -1" << endl;
        *total = 0;
    }
}

void In(Heap heap, int id, int casos, Hospital *hospital) {
    int i;
    i = 0;
    while(i < heap.size){
        if(id == hospital[i].id){
        hospital[i].casos += casos;
        cout << hospital[i].casos << endl;
        hospital = buildHeap(heap.size , heap, hospital);
        i = heap.size;    
        }
        i++;
    }
}

void Out(Heap heap, int id, int casos, Hospital *hospital) {
    int i;
    i = 0;
    while(i < heap.size){
        if(id == hospital[i].id){
        hospital[i].casos -= casos;
        cout << hospital[i].casos << endl;
        hospital = buildHeap(heap.size, heap, hospital);
        i = heap.size;
        }
        i++;
    }
}

int Pay(Heap heap, Hospital *hospital) {
    int R, i, pagaC;

    cin >> pagaC;
    i = 0;
    while (i <= pagaC) {
        if (hospital[0].casos - 1 == 0) {
            hospital[0].casos--;
            R = i;
            cout << R << endl;
            break;
        } else {
            hospital[0].casos--;
        }
        hospital = buildHeap(heap.size, heap, hospital);
        R = i;
        if(hospital[0].casos == 0){
            R = i;
            cout << R << endl;
        }
        if(i == pagaC){
            cout << pagaC << endl;
            i = pagaC + 2;
        }
        i++;
    }
    return R;
}

int main() {
    int N, i, total = 0, casos = 0, id;
    Heap heap;
    string input;

    cin >> N;
    heap.hospital = (Hospital*)calloc(N, sizeof(Hospital));

    for (i = 0; i < N; i++) {
        cin >> casos;
        total += casos;
        heap.idTotal++;
        heap.size++;
        heap.hospital = heapInsert(heap.hospital, casos, heap.size-1, heap.idTotal-1);
    }

    while (input != "END") {
        cin >> input;
        if (input == "NEW") {
            cin >> id;
            cin >> casos;
            heap.size++;
            total += casos;
            heap.idTotal++;
            heap.hospital = (Hospital*) realloc(heap.hospital, heap.size*sizeof(Hospital));
            heap.hospital = heapInsert(heap.hospital, casos, heap.size-1, heap.idTotal-1);
            cout << heap.hospital[0].id << " " << heap.hospital[0].casos << endl;
        }
        else if (input == "DEL") {
            cin >> id;
            Del(heap.hospital, id, &total, &heap);
        }
        else if (input == "IN") {
            cin >> id >> casos;
            total += casos;
            In(heap, id, casos, heap.hospital);
        }
        else if (input == "OUT") {
            cin >> id >> casos;
            total -= casos;
            Out(heap, id, casos, heap.hospital);
        }
        else if (input == "PAY") {
            int R = Pay(heap, heap.hospital);
            total = total - R;
        }
        else {
            cout << total << endl;
        }
    }
    return 0;
}