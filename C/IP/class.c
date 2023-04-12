#include <stdio.h>

typedef struct {
  int id;
  char nome[50];
} Monitores;

typedef struct {
  int q1, q2, q3;
} Lista;

typedef struct {
  char nome[50];
  int monitor;
  Lista listas[3];
  int concluidos;
} Classe;

int main() {
  Classe estudante[10];
  Monitores monitor[3];
  int qtd = 0, choice;
  
  scanf("%d", &qtd);
  for (int i = 0; i < qtd; i++) {
    scanf(" %s", monitor[i].nome);
    scanf("%d", &monitor[i].id);
  }

  scanf("%d", &qtd);
  for (int i = 0; i < qtd; i++) {
    scanf(" %s", estudante[i].nome);
    scanf("%d", &estudante[i].monitor);
    estudante[i].concluidos = 0;
  }

  for (int i = 0; i < qtd; i++) {
    for (int j = 0; j < 3; j++) {
      scanf("%d", &estudante[i].listas[j].q1);
      if (estudante[i].listas[j].q1) 
        estudante[i].concluidos++;
      
      scanf("%d", &estudante[i].listas[j].q2);
      if (estudante[i].listas[j].q2) 
        estudante[i].concluidos++;
      
      scanf("%d", &estudante[i].listas[j].q3);
      if (estudante[i].listas[j].q3) 
        estudante[i].concluidos++;
    }
  }

  scanf("%d", &choice);
  for (int i = 0; i < qtd; i++) {
    if (estudante[i].listas[choice-1].q3) {
      estudante[i].concluidos++;
    }
  }

  for (int i = 0; i < qtd; i++) {
    printf("%s\n", estudante[i].nome);
    for (int j = 0; j < qtd; j++) {
      if (estudante[i].monitor == monitor[j].id) {
        printf("%s\n", monitor[j].nome);
      }
    }
    printf("%d\n", estudante[i].concluidos);
  }
}

