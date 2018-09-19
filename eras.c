/* author: Kamil Kos */
	
#include <omp.h>
#include <stdlib.h>

char* primes = 0;
unsigned int length = 0;

//funkcja sprawdza czy dana liczba jest pierwsza
char is_prime(unsigned int number) {
	unsigned int i;
	for(i=2; i<number; i++)
		if(!(number%i))
			return 0;
	return 1;
}

/*
funkcja sprawdza poprawność wyniku algorytmu za pomocą funkcji powyżej,
usprawnienie działania przy użyciu OpenMP
*/
char check_all_primes() {
	unsigned int i;
	char good = 1;
	#pragma omp parallel for
	for(i=2; i<length; i++)
		if(is_prime(i) != primes[i])
			good = 0;
	return good;
}
