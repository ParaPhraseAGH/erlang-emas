#include "erl_nif.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "rastrigin.h"


// #define SEED 0
// #define DEBUG

#define CHECK(env, obj) if (!(obj)){return enif_make_badarg((env));}


extern double   fitness_rastrigin(Solution* sol);
extern double   mutate_feature(double feature, double mutation_range);
extern void     mutate(Solution* prev, Solution* out, double range, double rate);
extern void     recombine(Solution** parents, Solution** children);
extern double   randdouble(double lower, double upper);
extern void     print_solution(Solution* sol, const char* desc);
extern unsigned int get_seed();

// ErlNifResourceType* SOL_TYPE;

static void solution_dtor(ErlNifEnv* env, void* obj){
    Solution* sol = (Solution*) obj;
    if (sol != NULL && sol->genotype != NULL){
        free(sol->genotype);
    }
}

static int nif_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info){
    const char* mod = "rastrigin_ops_nif";
    const char* name = "Solution";
    int flags = ERL_NIF_RT_CREATE; // | ERL_NIF_RT_TAKEOVER;

    *priv = enif_open_resource_type(env, mod, name, solution_dtor, flags, NULL);

    // SOL_TYPE = *priv;

    #ifdef SEED
    srand(SEED);
    #else 
    srand(get_seed());
    // srand(time(NULL));
    #endif

    return 0;
}

static ERL_NIF_TERM evaluate_solution(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    // long diff;
    // struct timespec start, end;
    // clock_gettime(CLOCK_MONOTONIC, &start);

    ErlNifResourceType* sol_type;
    Solution* sol;
    double fitness;

    CHECK(env, argc == 1)

    sol_type = (ErlNifResourceType*) enif_priv_data(env);
    CHECK(env, sol_type)

    CHECK(env, enif_get_resource(env, argv[0], sol_type, (void**) &sol))

    #ifdef DEBUG
    print_solution(sol,"Genotype");
    #endif

    fitness = fitness_rastrigin(sol);

    // clock_gettime(CLOCK_MONOTONIC, &end);
    // diff = CLOCKS_PER_SEC * (end.tv_sec - start.tv_sec) + end.tv_nsec - start.tv_nsec;
    // printf("eval=%llu\n", (long long unsigned int) diff);


    return enif_make_double(env, fitness);

}



static ERL_NIF_TERM recombine_solutions(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    // long diff;
    // struct timespec start, end;
    // clock_gettime(CLOCK_MONOTONIC, &start);

    ERL_NIF_TERM terms[2];
    ErlNifResourceType* sol_type;
    Solution *sols[2], *new_sols[2]; // arrays of two pointers of type Solution*
    unsigned int i;
    unsigned int len;

    CHECK(env, argc == 2)

    sol_type = (ErlNifResourceType*) enif_priv_data(env);
    CHECK(env, sol_type)

    // read parent solutions
    for (i=0; i<2; i++){
        CHECK(env, enif_get_resource(env, argv[i], sol_type, (void**) &sols[i]))
    }

    #ifdef DEBUG
    print_solution(sols[0],"Genotype1");
    print_solution(sols[1],"Genotype2");
    #endif
    
    // allocate 2 child solution structures
    len = sols[0]->len;
    for (i=0; i<2; i++){
        new_sols[i] = (Solution*) enif_alloc_resource(sol_type, sizeof(Solution));
        CHECK(env, new_sols[i])
        
        terms[i] = enif_make_resource(env, new_sols[i]);
        CHECK(env,terms[i])
        enif_release_resource(new_sols[i]);

        new_sols[i]->len = len;
        new_sols[i]->genotype = (double*) malloc(sizeof(double)*len);
    }


    recombine(sols, new_sols);

    #ifdef DEBUG
    print_solution(new_sols[0],"RecombinedGenotype1");
    print_solution(new_sols[1],"RecombinedGenotype2");
    #endif
    
    // clock_gettime(CLOCK_MONOTONIC, &end);
    // diff = CLOCKS_PER_SEC * (end.tv_sec - start.tv_sec) + end.tv_nsec - start.tv_nsec;
    // printf("reco=%llu\n", (long long unsigned int) diff);


    return enif_make_tuple2(env, terms[0], terms[1]);
}


static ERL_NIF_TERM mutate_solution(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    // struct timespec start, end;
    // long diff;
    // clock_gettime(CLOCK_MONOTONIC, &start);

    ERL_NIF_TERM term;
    ErlNifResourceType* sol_type;
    Solution *sol, *mut_sol;
    // unsigned int i;
    double range, rate;

    CHECK(env, argc == 3)

    sol_type = (ErlNifResourceType*) enif_priv_data(env);
    CHECK(env, sol_type)

    CHECK(env, enif_get_resource(env, argv[0], sol_type, (void**) &sol))
    CHECK(env, enif_get_double(env, argv[1], &range))
    CHECK(env, enif_get_double(env, argv[2], &rate))

    mut_sol = (Solution*) enif_alloc_resource(sol_type, sizeof(Solution));
    CHECK(env, mut_sol)
    
    term = enif_make_resource(env, mut_sol);
    CHECK(env,term)
    enif_release_resource(mut_sol);

    mut_sol->len = sol->len;
    mut_sol->genotype = (double*) malloc(sizeof(double)*sol->len);

    mutate(sol, mut_sol, range, rate);

    #ifdef DEBUG
    print_solution(sol, "Genotype");
    print_solution(mut_sol, "MutatedGenotype");
    #endif

    // clock_gettime(CLOCK_MONOTONIC, &end);
    // diff = CLOCKS_PER_SEC * (end.tv_sec - start.tv_sec) + end.tv_nsec - start.tv_nsec;
    // printf("mut=%llu\n", (long long unsigned int) diff);

    return term;

}

static ERL_NIF_TERM create_solution(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    // struct timespec start, end;
    // long diff;
    // clock_gettime(CLOCK_MONOTONIC, &start);

    ERL_NIF_TERM term;
    ErlNifResourceType* sol_type;
    Solution* sol;
    unsigned int len;
    unsigned int i;

    CHECK(env, argc == 1)
    CHECK(env, enif_get_uint(env, argv[0], &len))

    sol_type = (ErlNifResourceType*) enif_priv_data(env);
    CHECK(env, sol_type)

    sol = (Solution*) enif_alloc_resource(sol_type, sizeof(Solution));
    CHECK(env, sol)
    
    term = enif_make_resource(env, sol);
    CHECK(env,term)
    enif_release_resource(sol);

    sol->len = len;
    sol->genotype = (double*) malloc(sizeof(double)*len);
    for (i=0;i<len;i++){
        sol->genotype[i] = randdouble(-50.0, 50.0);
    }

    // clock_gettime(CLOCK_MONOTONIC, &end);

    // diff = CLOCKS_PER_SEC * (end.tv_sec - start.tv_sec) + end.tv_nsec - start.tv_nsec;
    // printf("create=%llu\n", (long long unsigned int) diff);


    return term;
}


static ErlNifFunc nif_funcs[] = {
    {"solution", 1, create_solution},
    {"evaluation", 1, evaluate_solution},
    {"mutation", 3, mutate_solution},
    {"recombination", 2, recombine_solutions}
};


ERL_NIF_INIT(rastrigin_nif_ops, nif_funcs, nif_load, NULL, NULL, NULL)