/* file odeparms.c */

#include <R.h>
#include <math.h>

static double parms[18];

#define f_ovo_a parms [0]
#define f_trans_u2l  parms [1]  
#define f_trans_l2p parms [2] 
#define f_trans_p2a parms [3] 
#define ma  parms [4] 
#define mu  parms [5]   
#define mp  parms [6]  
#define ml  parms [7] 
#define alfa_l  parms [8] 
#define alfa_a  parms [9] 
#define ef  parms [10]
#define egn  parms [11]
#define N_ini_a  parms [12]
#define N_ini_p  parms[13]
#define N_ini_l  parms[14]
#define N_ini_u  parms[15]
#define sexratio  parms[16]
#define deltatime  parms[17]


/* initializer */

void initmod(void (* odeparms)(int *, double *))
{
int N=18;
odeparms(&N, parms);
}



#define L1 y[0]
#define L3 y[1]
#define L4 y[2]
#define L5 y[3]
#define L6 y[4]
#define L7 y[5]
#define L8 y[6]
#define L10 y[7]

#define dL1 ydot[0]
#define dL3 ydot[1]
#define dL4 ydot[2]
#define dL5 ydot[3]
#define dL6 ydot[4]
#define dL7 ydot[5]
#define dL8 ydot[6]
#define dL10 ydot[7]


/*
 Derivatives and 8 output variables */

void derivs (int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{
if (ip[0] <1) error("nout should be at least 1");
  L1=(f_ovo_a*( N_ini_a + (sexratio*L8) - L10 - L1 ))/ deltatime; 
  L3=(mu*(N_ini_u+egn*L1-L3))/ deltatime; 
  L4=(f_trans_u2l*(N_ini_u+egn*L1-L3-L4))/deltatime; 			
  L5=(f_trans_l2p*(N_ini_l+L4-L5-L6))/deltatime; 					   
  L6=(ml*(N_ini_l+L4-L5-L6) + (alfa_l*powl((N_ini_l+L4-L5-L6),2)))/deltatime;      
  L7=((mp+sexratio*f_trans_p2a)*(N_ini_p+L5-L7-L8)/deltatime);       
  L8=((f_trans_p2a *(sexratio*N_ini_p)+L5-L7-L8))/deltatime; 
  L10=(ma*(N_ini_a+(sexratio*L8)-L10-L1)+ (alfa_a*(N_ini_a+(sexratio*L8)-L10-L1)*(N_ini_a+(sexratio*L8)-L10-L1)))/deltatime;
  dL1=L1;dL3=L3;dL4=L4;dL5=L5;dL6=L6;dL7=L7;dL8=L8;dL10=L10;
      
}

/* END file odeparms.c */
