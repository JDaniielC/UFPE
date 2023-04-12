#include <iostream>

using namespace std;
#define MAX 2147483647;

struct Vizinho {
    int vizinhos;
    double height;
};

struct Ponto {
    int x, y;
    bool coin = false;
    int qtdVizinhos;
    Vizinho *vizinhos;
};

double getEnergy(Ponto *plataforma, int lugar, int vizinho){
    double X = plataforma[vizinho].x - plataforma[lugar].x; 
    double Y = plataforma[vizinho].y - plataforma[lugar].y; 
    X = X*X;
    Y = Y*Y;
    double height = X + Y;
    
    if(plataforma[vizinho].coin)
        height = height * -1;
    
    return height;
}

void Algoritmo(Ponto *plataforma, int lugar, int N){
    int precursores[N];
    double dK[N], D[N]; 

    for(int i = 0; i < N; i++){
        precursores[i] = -1;
        D[i] = MAX;
    }
    D[lugar] = 0;

    for(int k = 1; k < N; k++){
        for(int u = 0; u < N; u++){
            dK[u] = D[u];
        }
        for (int i = 0; i < N; i++) {
            for(int u = 0; u < plataforma[i].qtdVizinhos; u++){
                if((dK[i] + plataforma[i].vizinhos[u].height) < D[plataforma[i].vizinhos[u].vizinhos]){
                    D[plataforma[i].vizinhos[u].vizinhos] = dK[i] + plataforma[i].vizinhos[u].height;
                    precursores[plataforma[i].vizinhos[u].vizinhos] = i;
                }
            }
        }
    }
    for (int i = 0; i < N; i++) {
        for(int u = 0; u < plataforma[i].qtdVizinhos; u++){
            if((D[i] + plataforma[i].vizinhos[u].height) < dK[plataforma[i].vizinhos[u].vizinhos]){
                cout << "LOOP" << endl;
                return;
            }
        }
    }

    int Teste[N], place = 0, total = N-1, i;
    cout << D[total] << " ";

    for (i = total; i < N && i != 0; place++) {
        Teste[place] = i;
        i = precursores[i];
    }
    if (i == 0) place = place - 1;

    cout << "0" << " ";

    for (i = 0; place > -1; place--) {
        if (place) cout << Teste[place] << " ";
        else cout << Teste[place];
    }
    printf("\n");
}

int main() {
    int T, N, M, lugar, qtdVizinhos, vizinhos;

    cin >> T;
    for (int i = 0; i < T; i++) {
        cin >> N;
        Ponto *pontos = (Ponto*) calloc(N, sizeof(Ponto));;
        for (int j = 0; j < N; j++) 
            cin >> pontos[j].x >> pontos[j].y;
        
        cin >> M;
        for (int j = 0; j < M; j++) {
            cin >> lugar;
            pontos[lugar].coin = true;
        }   

        for (int j = 0; j < N; j++) {
            cin >> qtdVizinhos;
            pontos[j].qtdVizinhos = qtdVizinhos;
            pontos[j].vizinhos = (Vizinho*) calloc(qtdVizinhos, sizeof(Vizinho));
            for (int k = 0; k < qtdVizinhos; k++) {
                cin >> vizinhos;
                pontos[j].vizinhos[k].vizinhos = vizinhos;
                pontos[j].vizinhos[k].height = getEnergy(pontos, j, vizinhos);
            }
        }

        Algoritmo(pontos, 0, N);
        for (int j = 0; j < N; j++) {
            free(pontos[j].vizinhos);
        }
        free(pontos);
    }
}