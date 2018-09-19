/* author: Kamil Kos */

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

extern char* primes; 
extern unsigned int length;

extern char check_all_primes();

//alokacja pamięci i inicjalizacja całej tablicy
static ERL_NIF_TERM init_array_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned int n;
	if (!enif_get_uint(env, argv[0], &n)) {
		return enif_make_badarg(env);
    }
    length = n + 1;
    primes = enif_alloc(length*sizeof(char));
    memset(primes, 1, length);
    primes[0] = 0;
    primes[1] = 0;
    
    return enif_make_atom(env, "ok");
}

//sprawdzenie poprawności wyniku
static ERL_NIF_TERM check_array_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

	return check_all_primes()?(enif_make_atom(env, "ok")):(enif_make_atom(env, "not ok"));
}

//zwolnienie pamięci
static ERL_NIF_TERM free_mem_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	if(length)
		enif_free(primes);
	length = 0;
	
	return enif_make_atom(env, "ok");
}

//oznacza daną liczbę jako nie-pierwszą (wykreśla z tablicy liczb pierwszych)
static ERL_NIF_TERM cross_out_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned int num;
	if (!enif_get_uint(env, argv[0], &num)) {
		return enif_make_badarg(env);
    }
    
    primes[num] = 0;
    
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"init_array", 1, init_array_nif},
    {"check_array", 0, check_array_nif},
    {"free_mem", 0, free_mem_nif},
    {"cross_out", 1, cross_out_nif}
};

ERL_NIF_INIT(eras, nif_funcs, NULL, NULL, NULL, NULL)
