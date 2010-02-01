# include <R.h>
# include <Rdefines.h>
# include <Rmath.h>
# include <stdio.h>
# include <stdlib.h>
# include <float.h>

void printMatrix(double *mTraj,int *nbCol, int *nbLigne);
void printMatrixInt(int *mTraj,int *nbCol, int *nbLigne);

// all distance 'Traj' are optimized to work with trajectories
static double manhattanTraj(double *x,double *y,int *taille);
static double euclideanTraj(double *x,double *y,int *taille);
static double minkowskiTraj(double *x,double *y,int *taille,double *power);
static double maximumTraj(double *x,double *y,int *taille);
static double canberraTraj(double *x,double *y,int *taille);
static double binaryTraj(double *x,double *y,int *taille);

// distance that switch to one of the distance'Traj'
void distanceTraj(double *x, double *y, int *taille, double *power, double *dist);


// Frechet Distance (max & sum)
void distFrechet(double *x,double *y,int *tailleX, int *tailleY, int *methodMax, int *distanceTraj, double *dist);
void pathFrechet(double *x,double *y,int *tailleX, int *tailleY, int *methodMax, int *distanceTraj, double *dist, int *bestPath, int *pathLength);

// Frechet Distance and best path (max & sum)
/*void pathFrechetSum(double *x,double *y,int *tailleX, int *tailleY, double *dist, int *bestPath, int *pathLength);
void pathFrechetMax1D(double *x,double *y,int *tailleX, int *tailleY, double *dist, int *bestPath, int *pathLength);
void pathFrechetSum1D(double *x,double *y,int *tailleX, int *tailleY, double *dist, int *bestPath, int *pathLength);
*/




