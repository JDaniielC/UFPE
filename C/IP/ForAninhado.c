#include<stdio.h>
#include<stdlib.h>
int main(){
    //e=palheta; eg=cordas g=pedal
    int dinheiro;
    int e1,e2,eg3;
    int eg4,g5,g6;
    int verificador = 0;

    scanf("%d",&dinheiro);
    scanf("%d %d %d", &e1, &e2, &eg3);
    scanf("%d %d %d", &eg4, &g5, &g6);

   for(int z = e1; z <= e2; z = z + 1 ){//Externo - E
       for(int x = g5; x <= g6; x = x + 1){ // Médio - M
           for(int y = eg3; y <= eg4; y = y + 1){ // Interno - I
               if (z+x+y == dinheiro && verificador == 0) {
                   printf("S\n%d %d %d\n", z, y, x);
                   verificador = 1;
               } 
           }
       }
   }
   if (verificador == 0) {
       printf("N\n");
   }

}