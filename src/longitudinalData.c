# include "longitudinalData.h"

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))

/* Matrix are in line first :
0 1 2 3
4 5 6 7 */
/*

void printMatrix(double *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %f",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}

void printMatrixInt(int *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %i",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}


*/


/* *****************************************************
****************** Distances for traj ******************
***************************************************** */

static double manhattanTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int nbNA=0, i=0;
    double difference=0.0;

    for(i=0 ; i<*taille ; i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = fabs(x[i]-y[i]);
	    if(!ISNAN(difference)){
		dist += difference;
	    }else{}
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    return(dist);
}

static double euclideanTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int nbNA=0, i=0;
    double difference=0.0;

    for(i=0 ; i<*taille ; i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = x[i]-y[i];
	    if(!ISNAN(difference)){
		dist += difference * difference;
	    }else{}
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    dist = pow(dist,0.5);
    return(dist);
}

static double minkowskiTraj(double *x,double *y,int *taille, double *power){
    double dist = 0.0;
    int nbNA = 0, i=0;
    double difference=0.0;

    for(i=0;i<*taille;i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = fabs(x[i]-y[i]);
	    if(!ISNAN(difference)){
		dist += pow(difference,*power);
	    }else{}
	}else{
	    nbNA +=1;
	}
    };
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    dist = pow( dist, 1/ *power);
    return(dist);
}


static double maximumTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int i=0;
    Rboolean onlyNA = TRUE;
    double difference=0.0;

    dist = -DBL_MAX;
    for(i = 0 ; i < *taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    difference = fabs(x[i] - y[i]);
	    if(!ISNAN(difference)) {
		if(difference > dist){
		    dist = difference;
		    onlyNA = FALSE;
		}else{};
	    }else{
	    };
	};
    };
    if(onlyNA){return(NA_REAL);}else{}
    return(dist);
}


static double canberraTraj(double *x, double *y, int *taille){//int nr, int nc, int i1, int i2)
    double dist=0.0;
    int nbNA=0,i=0;
    double fraction=0.0,sum=0.0;

    for(i = 0 ; i < *taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    sum = fabs(x[i]) + fabs(y[i]);
	    if(sum==0){
		fraction = 1;
	    }else{
		fraction = fabs(x[i] - y[i])/sum;
	    };
	    if(!ISNAN(fraction)){
		dist += fraction;
	    }else{};
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA == *taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = dist * *taille/(*taille-nbNA);}else{};
    return dist;
}


static double binaryTraj(double *x, double *y, int *taille){
    double dist=0.0;
    int nbNA=0,i=0;
    int numerator=0,denumerator=0;

    for(i = 0 ; i < *taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    if(!both_FINITE(x[i], y[i])) {
		nbNA++;
	    }else{
		if(x[i] || y[i]) {
		    denumerator++;
		    if( ! (x[i] && y[i]) ){numerator++;}else{};
		}else{};
	    }
	}else{
	    nbNA++;
	}
    }

    if(denumerator==0){return NA_REAL;}else{};
    dist = (double)numerator/(double)denumerator;
    return(dist);
}



void distance(double *x,double *y,int *taille, double *power, double *dist){
    *dist = 0.0;
    if(*power==2.0){
	*dist = euclideanTraj(x,y,taille);
    }else{
	if(*power==1.0){
	    *dist = manhattanTraj(x,y,taille);
	}else{
	    if(*power==R_PosInf){
		*dist = maximumTraj(x,y,taille);
	    }else{
		if(*power==-1){ // -1 code canberra
		    *dist = canberraTraj(x,y,taille);
		}else{
		    if(*power==-2){ // -2 code binary
			*dist = binaryTraj(x,y,taille);
		    }else{
			*dist = minkowskiTraj(x,y,taille,power);
		    }
		}
	    }
	}
    }
}






/* *****************************************************
****************** Distances  frechet ******************
***************************************************** */

// Y in line, X in column
void distFrechet(double *x,double *y,int *tailleX, int *tailleY, int *methodMax, int *distanceTraj, double *dist){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;
/*    for(i=0;i< *tailleX * *tailleY;i++){
	matriceDist[i] = 0.0;
	matriceFrechet[i]=0.0;
    };
    Rprintf("Distance\n");
    printMatrix(matriceDist,tailleX,tailleY);
    Rprintf("Frechet\n");
    printMatrix(matriceFrechet,tailleX,tailleY);*/
    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    if(*distanceTraj){
		matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    }else{
		matriceDist[i *  *tailleY + j] = pow( (i-j)*(i-j)+(x[i]-y[j])*(x[i]-y[j]),0.5);
	    }

	    if(i>0){
		if(j>0){
		    if(*methodMax){
			matriceFrechet[i *  *tailleY + j] =
			    fmax2(
				fmin2(fmin2(matriceFrechet[i *  *tailleY + j-1],matriceFrechet[(i-1) *  *tailleY + j]),matriceFrechet[(i-1) *  *tailleY + (j-1)]),
				matriceDist[i *  *tailleY + j]
			    );
		    }else{
			matriceFrechet[i *  *tailleY + j] =
			    fmin2(fmin2(matriceFrechet[i *  *tailleY + j-1],matriceFrechet[(i-1) *  *tailleY + j]),matriceFrechet[(i-1) *  *tailleY + (j-1)]) +
			    matriceDist[i *  *tailleY + j];
		    };
/*		    Rprintf("\nYYYYYYYYY\ni %i \nj %i \n taille %i \nij %f \nij-1 %f \ni-1j %f \nij %f\n",
			    i,j,*tailleY,matriceFrechet[i *  *tailleY + j-1],
			    matriceFrechet[(i-1) *  *tailleY + j],matriceFrechet[(i-1) *  *tailleY + (j-1)],
			    matriceDist[i *  *tailleY + j]);*/
		}else{
		    if(*methodMax){
			matriceFrechet[i *  *tailleY] = fmax2( matriceFrechet[(i-1) *  *tailleY] , matriceDist[i *  *tailleY] );
		    }else{
			matriceFrechet[i *  *tailleY] = matriceFrechet[(i-1) *  *tailleY] + matriceDist[i *  *tailleY];
		    };
		};
	    }else{
		if(j>0){
		    if(*methodMax){
			matriceFrechet[j] = fmax2(matriceFrechet[j-1] , matriceDist[j] );
		    }else{
			matriceFrechet[j] = matriceFrechet[j-1] + matriceDist[j];
		    };
		}else{
		    matriceFrechet[0] = matriceDist[0];
		};
	    };
/*	    Rprintf("************ Distance\n");
	    printMatrix(matriceDist,tailleX,tailleY);
	    Rprintf("Frechet\n");
	    printMatrix(matriceFrechet,tailleX,tailleY);*/

	};
    };
/*    Rprintf("************ Distance\n");
    printMatrix(matriceDist,tailleY,tailleX);
    Rprintf("Frechet\n");
    printMatrix(matriceFrechet,tailleY,tailleX);*/
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
}


/*void distFrechetSum(double *x,double *y,int *tailleX, int *tailleY, double *dist){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    matriceDist[i *  *tailleY + j] = pow((i-j)*(i-j)+(x[i]-y[j])*(x[i]-y[j]),0.5);
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			matriceDist[i *  *tailleY + j] +
			fmin2(
			    fmin2(
				matriceFrechet[i *  *tailleY + j-1],
				matriceFrechet[(i-1) *  *tailleY + j]
			    ),
			    matriceFrechet[(i-1) *  *tailleY + (j-1)]
			);
		}else{
		    matriceFrechet[i *  *tailleY] = matriceDist[i *  *tailleY] + matriceFrechet[(i-1) *  *tailleY];
		};
	    }else{
		if(j>0){
		    matriceFrechet[j] = matriceDist[j] + matriceFrechet[j-1];
		}else{
		    matriceFrechet[0] = matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};


// Y in line, X in column
void distFrechetMax1D(double *x,double *y,int *tailleX, int *tailleY, double *dist){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			fmax2(
			    fmin2(
				fmin2(
				    matriceFrechet[i *  *tailleY + j-1],
				    matriceFrechet[(i-1) *  *tailleY + j]
				),
				matriceFrechet[(i-1) *  *tailleY + (j-1)]
			    ),
			    matriceDist[i *  *tailleY + j]
			);
		}else{
		    matriceFrechet[i *  *tailleY]=
			fmax2(matriceFrechet[(i-1) *  *tailleY],
			    matriceDist[i *  *tailleY]
			);
		};
	    }else{
		if(j>0){
		    matriceFrechet[j]=
			fmax2(matriceFrechet[j-1],
			    matriceDist[j]
			);
		}else{
		    matriceFrechet[0]=matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};




void distFrechetSum1D(double *x,double *y,int *tailleX, int *tailleY, double *dist){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			matriceDist[i *  *tailleY + j] +
			fmin2(
			    fmin2(
				matriceFrechet[i *  *tailleY + j-1],
				matriceFrechet[(i-1) *  *tailleY + j]
			    ),
			    matriceFrechet[(i-1) *  *tailleY + (j-1)]
			);
		}else{
		    matriceFrechet[i *  *tailleY] = matriceDist[i *  *tailleY] + matriceFrechet[(i-1) *  *tailleY];
		};
	    }else{
		if(j>0){
		    matriceFrechet[j] = matriceDist[j] + matriceFrechet[j-1];
		}else{
		    matriceFrechet[0] = matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};

void distFrechetSum2D(double *x,double *y,int *tailleX, int *tailleY, double *dist){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    if(*tailleX==1){
		matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    }else{
		matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    };
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			matriceDist[i *  *tailleY + j] +
			fmin2(
			    fmin2(
				matriceFrechet[i *  *tailleY + j-1],
				matriceFrechet[(i-1) *  *tailleY + j]
			    ),
			    matriceFrechet[(i-1) *  *tailleY + (j-1)]
			);
		}else{
		    matriceFrechet[i *  *tailleY] = matriceDist[i *  *tailleY] + matriceFrechet[(i-1) *  *tailleY];
		};
	    }else{
		if(j>0){
		    matriceFrechet[j] = matriceDist[j] + matriceFrechet[j-1];
		}else{
		    matriceFrechet[0] = matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};



*/
/* *****************************************************
*************** Distances & Path frechet ***************
***************************************************** */

// Y in line, X in column
void pathFrechet(double *x,double *y,int *tailleX, int *tailleY, int *methodMax, int *distanceTraj, double *dist, int *bestPath, int *pathLength){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int matricePath[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;
    double movePQ=0,moveP=0,moveQ=0;
    const int directionPQ=0,directionP=1,directionQ=2,directionStart=3;
    *pathLength=0;

    for (i=0 ; i<(*tailleX + *tailleY)*2 ; i++){bestPath[i]=NA_INTEGER;};
/*    for(i=0;i< *tailleX * *tailleY;i++){
	matriceDist[i] = 0.0;
	matriceFrechet[i]=0.0;
    };
    Rprintf("Distance\n");
    printMatrix(matriceDist,tailleX,tailleY);
    Rprintf("Frechet\n");
    printMatrix(matriceFrechet,tailleX,tailleY);*/
    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){

	    if(*distanceTraj){
		matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    }else{
		matriceDist[i *  *tailleY + j] = pow( (i-j)*(i-j)+(x[i]-y[j])*(x[i]-y[j]),0.5);
	    }

	    if(i>0){
		if(j>0){
		    movePQ = matriceFrechet[(i-1) *  *tailleY + (j-1)];
		    moveP = matriceFrechet[(i-1) *  *tailleY + j];
		    moveQ = matriceFrechet[i *  *tailleY + (j-1)];
		    if(movePQ<=moveP){
			if(movePQ<=moveQ){
			    if(*methodMax){
				matriceFrechet[i *  *tailleY + j] = fmax2(movePQ,matriceDist[i *  *tailleY + j]);
			    }else{
				matriceFrechet[i *  *tailleY + j] = movePQ + matriceDist[i *  *tailleY + j];
			    };
			    matricePath[i *  *tailleY + j]=directionPQ;
			}else{
			    if(*methodMax){
				matriceFrechet[i *  *tailleY + j] = fmax2(moveQ,matriceDist[i *  *tailleY + j]);
			    }else{
				matriceFrechet[i *  *tailleY + j] = moveQ + matriceDist[i *  *tailleY + j];
			    };
			    matricePath[i *  *tailleY + j]=directionQ;
			};
		    }else{
			if(moveP<=moveQ){
			    if(*methodMax){
				matriceFrechet[i *  *tailleY + j] = fmax2(moveP,matriceDist[i *  *tailleY + j]);
			    }else{
				matriceFrechet[i *  *tailleY + j] = moveP + matriceDist[i *  *tailleY + j];
			    };
			    matricePath[i *  *tailleY + j] = directionP;
			}else{
			    if(*methodMax){
				matriceFrechet[i *  *tailleY + j] = fmax2(moveQ,matriceDist[i *  *tailleY + j]);
			    }else{
				matriceFrechet[i *  *tailleY + j] = moveQ + matriceDist[i *  *tailleY + j];
			    };
			    matricePath[i *  *tailleY + j]=directionQ;
			};
		    };

/*		    Rprintf("\nYYYYYYYYY\ni %i \nj %i \n taille %i \nij %f \nij-1 %f \ni-1j %f \nij %f\n",
			    i,j,*tailleY,matriceFrechet[i *  *tailleY + j-1],
			    matriceFrechet[(i-1) *  *tailleY + j],matriceFrechet[(i-1) *  *tailleY + (j-1)],
			    matriceDist[i *  *tailleY + j]);*/
		}else{
		    if(*methodMax){
			matriceFrechet[i *  *tailleY] = fmax2(matriceFrechet[(i-1) *  *tailleY],matriceDist[i *  *tailleY]);
		    }else{
			matriceFrechet[i *  *tailleY] = matriceFrechet[(i-1) *  *tailleY] + matriceDist[i *  *tailleY];
		    };
		    matricePath[i *  *tailleY] = directionP;
		};
	    }else{
		if(j>0){
		    if(*methodMax){
			matriceFrechet[j] = fmax2(matriceFrechet[j-1],matriceDist[j]);
		    }else{
			matriceFrechet[j] = matriceFrechet[j-1] + matriceDist[j];
		    };
		    matricePath[j] = directionQ;
		}else{
		    matriceFrechet[0]=matriceDist[0];
		    matricePath[0]=directionStart;
		};
	    };
/*	    Rprintf("************ Distance\n");
	    printMatrix(matriceDist,tailleY,tailleX);
	    Rprintf("Frechet\n");
	    printMatrix(matriceFrechet,tailleY,tailleX);
	    Rprintf("Path\n");
	    printMatrixInt(matricePath,tailleY,tailleX);*/

	};
    };
/*    Rprintf("************ Distance\n");
    printMatrix(matriceDist,tailleY,tailleX);
    Rprintf("Frechet\n");
    printMatrix(matriceFrechet,tailleY,tailleX);
    Rprintf("Path\n");
    printMatrixInt(matricePath,tailleY,tailleX);
    printMatrix(matriceDist,tailleX,tailleY);
      printMatrix(matriceFrechet,tailleX,tailleY);*/

    i = *tailleX-1 ;
    j = *tailleY-1 ;

    bestPath[0]=i;
    bestPath[1]=j;
    while(i>0||j>0){
	if(matricePath[i *  *tailleY + j]==directionQ){
	    j+=-1;
	}else{
	    if(matricePath[i *  *tailleY + j]==directionP){
		i+=-1;
	    }else{
		i+=-1;
		j+=-1;
	    }
	}
	*pathLength+=1;
	bestPath[*pathLength*2] = i;
	bestPath[*pathLength*2+1] = j;
    }

//    Rprintf("BestPath\n");
//    *tailleY=*tailleY + *tailleX;
  //  *tailleX=2;
    //   printMatrixInt(bestPath,tailleX,tailleY);
    *pathLength+=1;
    //printMatrix(matriceFrechet,tailleY,tailleX);
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
}

/*
void pathFrechetSum(double *x,double *y,int *tailleX, int *tailleY, double *dist, int *bestPath, int *pathLength){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    matriceDist[i *  *tailleY + j] = pow((i-j)*(i-j)+(x[i]-y[j])*(x[i]-y[j]),0.5);
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			matriceDist[i *  *tailleY + j] +
			fmin2(
			    fmin2(
				matriceFrechet[i *  *tailleY + j-1],
				matriceFrechet[(i-1) *  *tailleY + j]
			    ),
			    matriceFrechet[(i-1) *  *tailleY + (j-1)]
			);
		}else{
		    matriceFrechet[i *  *tailleY] = matriceDist[i *  *tailleY] + matriceFrechet[(i-1) *  *tailleY];
		};
	    }else{
		if(j>0){
		    matriceFrechet[j] = matriceDist[j] + matriceFrechet[j-1];
		}else{
		    matriceFrechet[0] = matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};


// Y in line, X in column
void pathFrechetMax1D(double *x,double *y,int *tailleX, int *tailleY, double *dist, int *bestPath, int *pathLength){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			fmax2(
			    fmin2(
				fmin2(
				    matriceFrechet[i *  *tailleY + j-1],
				    matriceFrechet[(i-1) *  *tailleY + j]
				),
				matriceFrechet[(i-1) *  *tailleY + (j-1)]
			    ),
			    matriceDist[i *  *tailleY + j]
			);
		}else{
		    matriceFrechet[i *  *tailleY]=
			fmax2(matriceFrechet[(i-1) *  *tailleY],
			    matriceDist[i *  *tailleY]
			);
		};
	    }else{
		if(j>0){
		    matriceFrechet[j]=
			fmax2(matriceFrechet[j-1],
			    matriceDist[j]
			);
		}else{
		    matriceFrechet[0]=matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};




void pathFrechetSum1D(double *x,double *y,int *tailleX, int *tailleY, double *dist, int *bestPath, int *pathLength){

    double matriceDist[*tailleX * *tailleY];
    double matriceFrechet[*tailleX * *tailleY];
    int i=0,j=0;
    *dist=0.0;

    for(i=0 ;  i <  *tailleX ; i++){
        for(j=0 ; j <  *tailleY ; j++){
	    matriceDist[i *  *tailleY + j] = fabs(x[i]-y[j]);
	    if(i>0){
		if(j>0){
		    matriceFrechet[i *  *tailleY + j]=
			matriceDist[i *  *tailleY + j] +
			fmin2(
			    fmin2(
				matriceFrechet[i *  *tailleY + j-1],
				matriceFrechet[(i-1) *  *tailleY + j]
			    ),
			    matriceFrechet[(i-1) *  *tailleY + (j-1)]
			);
		}else{
		    matriceFrechet[i *  *tailleY] = matriceDist[i *  *tailleY] + matriceFrechet[(i-1) *  *tailleY];
		};
	    }else{
		if(j>0){
		    matriceFrechet[j] = matriceDist[j] + matriceFrechet[j-1];
		}else{
		    matriceFrechet[0] = matriceDist[0];
		};
	    };
	};
    };
    *dist = matriceFrechet[*tailleX * *tailleY - 1];
};


*/
