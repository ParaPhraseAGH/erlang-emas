#include "erl_nif.h"

#define INIT_FITNESS -999999
#define MAX(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
    double* fitness_var = (double*) enif_alloc(sizeof(int));
    if (fitness_var == NULL) {
        return enif_make_badarg(env);
    }
    *fitness_var = INIT_FITNESS;
    *priv = (void*) fitness_var;
    return 0;
}

static ERL_NIF_TERM delete_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double* priv = (double*) enif_priv_data(env);
    free(priv);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM get_value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double* priv = (double*) enif_priv_data(env);
    ERL_NIF_TERM fitness_atom = enif_make_atom(env, "fitness");
    ERL_NIF_TERM fitness_val = enif_make_double(env, *priv);
    ERL_NIF_TERM tuple = enif_make_tuple2(env, fitness_atom, fitness_val);
    return enif_make_list1(env, tuple);
}

static ERL_NIF_TERM update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double new_fitness;
    if (!enif_get_double(env, argv[1], &new_fitness)) {
	    return enif_make_badarg(env);
    }
    double* priv = (double*) enif_priv_data(env);
    double old_fitness = *priv;
    *priv = MAX(old_fitness, new_fitness);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM reset_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double* priv = (double*) enif_priv_data(env);
    *priv = INIT_FITNESS;
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"delete", 3, delete_nif},
    {"get_value", 4, get_value_nif},
    {"update", 4, update_nif},
    {"reset", 3, reset_nif}
};

ERL_NIF_INIT(emas_fitness_entry_nif, nif_funcs, &load, NULL, NULL, NULL)