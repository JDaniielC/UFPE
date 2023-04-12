#include <iostream>

using namespace std;
class Music {
    public:
        string nome = "None";
        int tempo = 0;
        int total = 0;
        int key = 0;
        void Increment() {
            total += tempo;
        }
};

int main() {
    bool verif = false;
    int M, t, count = 0;
    string input = "Begin", s;
    Music *spotify = nullptr;
    cin >> M;
    spotify = (Music*) calloc(M, sizeof(Music));
    
    while (input != "END") {
        cin >> input;
        if (input == "ADD") {
            if (count-1 >= M/2) {
                Music *aux = nullptr;
                aux = (Music*) calloc(M*2, sizeof(Music));
                int j = 0;

                for (int i = 0; i < M; i++) {
                    if (spotify[i].key != 0) {
                        aux[j] = spotify[i];
                        j++;
                    }
                }
                M = M*2 + 1;
                verif = false;

                spotify = (Music*) calloc(M, sizeof(Music));
                for (int i = 0; aux[i].key != 0; i++) {
                    verif = false;
                    for (int j = 0; verif != true; j++) {
                        int pos = (aux[i].key + j) % M;
                        if (spotify[pos].tempo == 0) {
                            spotify[pos] = aux[i];
                            verif = true;
                        } 
                    }
                }
                free(aux);
            }

            cin >> s >> t;

            int key = 0;
            for (int i = 0; i < s.length() ; i++) { 
                key += s[i]*i; 
            }
            verif = false;
            
            for (int i = 0; verif != true; i++) {
                int pos = (key + i) % M;
                if (spotify[pos].tempo == 0) {
                    spotify[pos].nome = s;
                    spotify[pos].tempo = t;
                    spotify[pos].key = key;
                    verif = true;
                    cout << s << " " << pos << endl;
                } 
            }
            count++;
        } else if (input == "PLAY") {
            cin >> s;

            int key = 0;
            for (int i = 0; i < s.length() ; i++) { key += s[i]*i; }
            verif = false;

            for (int i = 0; verif != true; i++) {
                int pos = (key + i) % M;
                if (s == spotify[pos].nome) {
                    spotify[pos].Increment();
                    cout << spotify[pos].nome << " " << spotify[pos].total << endl;
                    verif = true;
                }
            }
        } else if (input == "TIME") {
            cin >> s;

            int key = 0;
            for (int i = 0; i < s.length() ; i++) { key += s[i]*i; }
            verif = false;

            for (int i = 0; verif != true; i++) {
                int pos = (key + i) % M;
                if (s == spotify[pos].nome) {
                    cout << spotify[pos].nome << " " << spotify[pos].total << endl;
                    verif = true;
                }
            }
        } else { free(spotify);}
    }
}