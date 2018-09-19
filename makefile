CC = gcc-6
FLAGS = -fopenmp -Wall -pedantic -pedantic-errors -Werror
INCLUDE = -I \usr

eras_nif.so: eras_nif.c eras.c
	$(CC) -fpic -shared -o eras_nif.so eras_nif.c eras.c $(FLAGS) $(INCLUDE)
