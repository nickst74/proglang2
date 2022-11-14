#include <iostream>
#include <stack>
#include <bits/stdc++.h>

using namespace std;

#define MAX_VAL 10005
#define MAX_SPEED 30

typedef enum {N,Y} M;

typedef struct Mint{
    M flag;
    int val;
} MI;

int v[MAX_VAL];
MI dp[MAX_VAL+MAX_SPEED][MAX_SPEED];
int deacc, acc;
int size;

int dpf(int p, int sp){
    if(dp[p][sp].flag == N){
        dp[p][sp].flag = Y;
        if(p>size){
            dp[p][sp].val = 0;
        } else{
            int small = MAX_VAL;
            int max_speed = INT32_MAX; ///////////////////
            for(int i = 1; i < max(1,sp-deacc); i++)
                max_speed = min(max_speed, v[min(size,p+i)]);
            for(int i = max(1,sp-deacc); i<=sp+acc; i++){
                max_speed = min(max_speed,v[min(size,p+i)]);
                if(i>max_speed)
                    break;
                small = min(small,dpf(p+i, i));
            }
            dp[p][sp].val = 1 + small;

            //for(int i=0; i<size+1; i++){
            //    for(int j=0; j<30; j++){
            //        cout <<dp[i][j].val <<" ";
            //    }
            //    cout <<endl;
            //}
            //cout <<"\n\n";
        }
    }
    return dp[p][sp].val;
}

int main(){
    int cases;
    scanf("%d", &cases);
    for(int it=0; it<cases; it++){
        memset(dp,0,(MAX_VAL+MAX_SPEED)*MAX_SPEED*sizeof(MI));
        memset(v,0,MAX_VAL*sizeof(int));
        scanf("%d%d", &acc, &deacc);
        acc/=10;
        deacc/=10;
        int a,b;
        size=0;
        while((scanf("%d%d", &a, &b), a!=0 && b!=0)){
            b/=10;
            for(int i = 0; i<a; i++)
                v[++size] = b;
        }
        printf("%d\n", dpf(0,0));
    }
    return 0;
}
