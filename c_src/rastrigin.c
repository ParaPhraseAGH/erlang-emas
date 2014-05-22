#include "rastrigin.h"
#include "stdio.h"
#include "stdlib.h"

/// utility functions

unsigned int get_seed(){
    FILE *urandom; 
    unsigned int seed; 

    urandom = fopen ("/dev/urandom", "r"); 
    if (urandom == NULL) { 
        fprintf (stderr, "Cannot open /dev/urandom!\n"); 
        exit(1); 
    } 
    fread (&seed, sizeof(seed), 1, urandom); 
    fclose(urandom);
    return seed;
}

double randdouble(double lower, double upper){
    return lower + (upper - lower) * (((double) rand()) / (double) RAND_MAX);
}

void print_solution(Solution* sol, const char* desc){
    int i;
    printf("%s:",desc);
    for(i=0; i<sol->len; i++){
        printf("%.10f ", sol->genotype[i]);
    }
    printf("\n");
}
///

/// genetic operations

double fitness_rastrigin(Solution * sol){
    double s = 0.0;
    int i;

    for (i=0; i<sol->len; i++){
        double x = sol->genotype[i];
        s += x*x - 10.0*cos(M_TWOPI*x) + 10.0;
    }

    return -s;
}


double mutate_feature(double feature, double mutation_range){
    double range;
    range = randdouble(0.0, mutation_range);
    if (range < 0.2){
        range = 5.0;
    } else if (range < 0.4){
        range = 0.2;
    } else {
        range = 1.0;
    }
    return feature + range * tan(M_PI * randdouble(-0.5, 0.5));
}

void mutate(Solution* prev, Solution* out, double range, double rate){
    int i;
    for (i=0;i<prev->len;i++){
        if (randdouble(0.0,1.0) < rate){
            out->genotype[i] = mutate_feature(prev->genotype[i], range);
        } else {
            out->genotype[i] = prev->genotype[i];
        }
    }
}

void recombine(Solution** parents, Solution** children){
    int i;    
    // recombine parent solutions into 2 child solutions
    for (i=0;i<parents[0]->len;i++){
        double lower, upper;
        if (parents[0]->genotype[i] >= parents[1]->genotype[i]){
            lower = parents[1]->genotype[i];
            upper = parents[0]->genotype[i];
        } else {
            lower = parents[0]->genotype[i];
            upper = parents[1]->genotype[i];
        }
        children[0]->genotype[i] = randdouble(lower,upper);
        children[1]->genotype[i] = randdouble(lower,upper);
    }
}
