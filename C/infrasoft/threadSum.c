#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#define size 2

//gcc exemplo1.c -o teste -lpthread

int m1[size][size]={{-1,3},{4,2}};
int m2[size][size]={{1,2},{3,4}};
int mf[size][size];

struct data{
    int x;
    int y;
};

void* getvalue(void* arg){
    struct data* pos=(struct data*)arg;
    int px=pos->x;
    int py=pos->y;
    int sum=0;
    int i;
    for(i=0;i<size;i++){
        sum+=m1[px][i]*m2[i][py];
    }
    mf[px][py]=sum;
    pthread_exit(0);
}
int main(){
    int i;
    int j;

    pthread_t thread[size*size];
    struct data pos[size*size];

    for(i=0;i<size;i++){
        for(j=0;j<size;j++){
            pos[size*i + j].x=i;
            pos[size*i + j].y=j;

            pthread_create(&thread[size*i + j],NULL,getvalue,&pos[size*i + j]);
        }
    }

    for(i=0;i<size;i++){
        for(j=0;j<size;j++){
            pthread_join(thread[size*i + j],NULL);
            printf("[%d] ",mf[i][j]);
        }
        printf("\n");
    }
    return 0;
}