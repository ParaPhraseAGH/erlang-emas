#include "erl_nif.h"
#include "float.h"

#define INIT_FITNESS -DBL_MAX
#define MAX(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

typedef struct
{
    double fitness;
    ErlNifRWLock* lock;
} priv_data;


static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {

    priv_data* data_struct = (priv_data*) enif_alloc(sizeof(priv_data));
    data_struct->fitness = INIT_FITNESS;
    data_struct->lock = enif_rwlock_create("fitness_val_lock");

    *priv = (void*) data_struct;
    return 0;
}

static ERL_NIF_TERM delete_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    //priv_data* priv = (priv_data*) enif_priv_data(env);
    //free(priv);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM get_value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    priv_data* priv = (priv_data*) enif_priv_data(env);
    ERL_NIF_TERM fitness_atom = enif_make_atom(env, "fitness");
    ERL_NIF_TERM fitness_val = enif_make_double(env, priv->fitness);
    ERL_NIF_TERM tuple = enif_make_tuple2(env, fitness_atom, fitness_val);
    return enif_make_list1(env, tuple);
}

static ERL_NIF_TERM update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double new_fitness;
    if (!enif_get_double(env, argv[1], &new_fitness)) {
	    return enif_make_badarg(env);
    }
    priv_data* priv = (priv_data*) enif_priv_data(env);

    if (priv->fitness < new_fitness) {
        enif_rwlock_rwlock(priv->lock);
        priv->fitness = MAX(priv->fitness, new_fitness);
        enif_rwlock_rwunlock(priv->lock);
    }
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM reset_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    priv_data* priv = (priv_data*) enif_priv_data(env);
    priv->fitness = INIT_FITNESS;
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"delete", 3, delete_nif},
    {"get_value", 4, get_value_nif},
    {"update", 4, update_nif},
    {"reset", 3, reset_nif}
};

ERL_NIF_INIT(emas_fitness_entry_nif, nif_funcs, &load, NULL, NULL, NULL)
