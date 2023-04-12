// Aula de Ponteiros!
//------------------------------------------------------------------------------------
//Biblioteca: Lembra das expansões do The Sims que você pirateava? É isso
#include <stdio.h> //Escrever e guardar as parada
#include <stdlib.h> //Alocação dinâmica, arquivos, etc
#include <string.h> //Possibilidade de mexer com frases
//------------------------------------------------------------------------------------
//Váriaveis: Tipos de ferramentas que a gente vai usar (sempre seguido de nome)
//int - Números inteiros
//float, double - Números seguido por virgula (ponto flutuante)
//char - Carácter (De números, letras até símbolos)
//Outros

//Operadores: Como interagir com essas ferramentas
// +  -  /  *  % (módulo) 
//------------------------------------------------------------------------------------
//Toda estrutura de um historico de partidas
typedef struct {
  char nome[50];
  int nivel;
  int abates;
  int assist;
  int mortes;
  int ouro;
  int farm;
} Champion;

typedef struct {
  int abatesTotal;
  int mortesTotal;
  int assistTotal;
  int ouroTotal;
  int torre;
  int inibidor;
  int barao;
  int arauto;
  int dragao;
  Champion player[5];
} Equipe;

typedef struct {
  Equipe time[2];
  int tempo[2];
  int data[3];
  char tipo[50];
  char mapa[30];
} Partida;

//-------------------------------------------------------------------------------------
//Funções:
int calcularOuro(int farm) {
  int catapa = farm % 6;
  int ouro = ((farm/6) * 20) + catapa * 60;
  return ouro;
}

void mostrarPartida(Partida partida) {
  printf("--------------------------------------------------------------------------------------\n");
  printf("%s . %s . %d:%d ", partida.mapa, partida.tipo, partida.tempo[0], partida.tempo[1]);
  printf(". %d/%d/%d\n", partida.data[0], partida.data[1], partida.data[2]);
  printf("-------------------------------------------------------------------------------------\n");

  for (int j = 0; j < 2; j++) {
    if (j == 0) printf("EQUIPE 1");
    else printf("EQUIPE 2");
    printf("      %d / %d / %d", partida.time[j].abatesTotal, partida.time[j].mortesTotal, partida.time[j].assistTotal);
    printf("     %d\n", partida.time[j].ouroTotal);

    for (int i = 0; i < 5; i++) {
      printf("%d - %s - ", partida.time[j].player[i].nivel, partida.time[j].player[i].nome);
      printf("%d %d %d", partida.time[j].player[i].abates, partida.time[j].player[i].mortes, partida.time[j].player[i].assist);

      printf(" - %d - %d\n", partida.time[j].player[i].farm, partida.time[j].player[i].ouro);
    }

    printf("%d %d %d %d %d\n", partida.time[j].torre, partida.time[j].inibidor, partida.time[j].barao, partida.time[j].dragao, partida.time[j].arauto);

    printf("-----------------------------------------------------------------------------------\n");
  }
}

void criarPartida(int *i, Partida *partida) {
  //Ponteiro é uma estante que você tem que ficar criando cada espaço por vez
  //Agora eu chamo um marcenero para montar minha estante:
  //Malloc, Realloc e Calloc
  //Malloc é um marcenero que manda a estante dele pronta, mas pode vir com algumas coisas dele
  //Calloc é um que já monta toda, porém, ele limpa tudo, não vai vir com nada dele.
  //Realloc é um marceneiro que monta por partes, dependendo de quanto vai pagar.

  partida = (Partida*) realloc(partida, sizeof(Partida)*(*i+1)); 

  printf("Digite o tempo (mm:ss)\n");
  scanf("%d:%d", &partida[*i].tempo[0], &partida[*i].tempo[1]);

  printf("Digite a data (dd/mm/year)\n");
  scanf("%d/%d/%d", &partida[*i].data[0], &partida[*i].data[1], &partida[*i].data[2]);
  
  printf("Digite o tipo da partida (Normal, Ranqueada, Aram)\n");
  scanf(" %s", partida[*i].tipo);

  printf("Digite o mapa da partida Howling Abyss ou Summoner`\n");
  scanf(" %[^\n]", partida[*i].mapa);
  
  for (int j = 0; j < 2; j++) {
    //Condicionais:
    //if, else, else if (Se, senão)
    if (j == 0)
      printf("Vamos tratar da equipe Azul:\n");
    else
      printf("Vamos tratar da equipe Vermelha:\n");
     
    printf("Digite quantos dragões foram abatidos\n");
    scanf("%d", &partida[*i].time[j].dragao);

    printf ("Digite quantos arautos foram abatidos\n");
    scanf("%d", &partida[*i].time[j].arauto);

    printf ("Digite quantos barão foram abatidos\n");
    scanf("%d", &partida[*i].time[j].barao);

    printf ("Digite quantos inibidores foram destruídos\n");
    scanf("%d", &partida[*i].time[j].inibidor);

    printf ("Digite quantas torres foram destruídas\n");
    scanf("%d", &partida[*i].time[j].torre);

    for (int n = 0; n < 5; n++) {
      printf("Digite qual nome do campeão:\n");
      scanf(" %s", partida[*i].time[j].player[n].nome);

      printf("Digite o nivel do campeão\n");
      scanf("%d", &partida[*i].time[j].player[n].nivel);

      printf("Digite o número de abates\n");
      scanf("%d", &partida[*i].time[j].player[n].abates);

      printf("Digite o núemero de assistencias\n");
      scanf("%d", &partida[*i].time[j].player[n].assist);

      printf("Digite o número de mortes\n");
      scanf("%d", &partida[*i].time[j].player[n].mortes);

      printf("Digite o número de farm\n");
      scanf("%d", &partida[*i].time[j].player[n].farm);
      //Calcular o ouro de cada personagem de acordo com o farm:
      partida[*i].time[j].player[n].ouro = calcularOuro(partida[*i].time[j].player[n].farm);

    }
    partida[*i].time[j].abatesTotal = 0;
    partida[*i].time[j].mortesTotal = 0;
    partida[*i].time[j].assistTotal = 0;
    partida[*i].time[j].ouroTotal = 0;

    //Calcular status da equipe:
    for (int m = 0; m < 5; m++) {
      partida[*i].time[j].abatesTotal += partida[*i].time[j].player[m].abates;
      partida[*i].time[j].mortesTotal += partida[*i].time[j].player[m].mortes;
      partida[*i].time[j].assistTotal += partida[*i].time[j].player[m].assist;
      partida[*i].time[j].ouroTotal += partida[*i].time[j].player[m].ouro;
    }
  }
  mostrarPartida(partida[*i]);
  *i = *i+1;
}

int main(void) {
  //Vetores: Uma estante com gavetas e espaços pra por ferramentas
  //Declare uma váriavel e após o nome dela coloque [] colchetes
  //Com n números dentro do colchete
  //Ao fazer isso, está criando um vetor estático.
  //No caso, a estante não aumenta ou diminui de tamanho.
  //Onde o nome da várivel é a estante e o índice é a chave para mostrar o valor

  char champion[5] = {'a','k','a','l','i'} ;
  //printf("%c%c%c%c%c", champion[0], champion[1], champion[2], champion[3], champion[4]);

  //Loop: São funções de repetição
  //While, do While e for
  //Todos seguidos de parênteses que são os argumentos da função
  //Ou seja, neste caso, a condição para continuar repetindo
  //Após isto, chaves {}, no caso, o corpo da função, sua Ordem
  int i = 0;
  // while (i < 5) {
  //   printf("%c", champion[i]);
  //   i++;
  // }
  
  //scanf(" %s", champion); //Ele capturou e guardou na variável o que você escreveu (scan)
  //printf("IMPRIMIR: %s", champion); //Imprimiu a variável colegio
  Partida *partida = NULL;
  criarPartida(&i, partida);

  free(partida);
  return 0;
}