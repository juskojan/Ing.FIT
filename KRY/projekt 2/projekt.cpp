/***
	Jan Jusko
	Kryptografie 2
	2017/2018
***/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <gmp.h>
#include <unistd.h>

// Globals & namespace
gmp_randstate_t random_state;
using namespace std;

// Struct for RSA values
typedef struct RSAKeys {
	mpz_t m;
	mpz_t n;
	mpz_t e;
	mpz_t c;
	mpz_t d;
	mpz_t p;
	mpz_t q;
	mpz_t phi;
	mpz_t seed;
} RSAKeys;


// RSA class
class RSA {
private:
	RSAKeys rsa;

	// Generates random array of a specific size
	void genRandomArray(mpz_t arr, int size) {
		unsigned char buffer[size];
		for(int i = 0; i < size; i++) {
			buffer[i] = rand() % 255;
		}
		// Do not generate even numbers
		buffer[size - 1] |= 0x01;

		mpz_import(arr, size, 1, sizeof(buffer[0]), 0, 0, buffer);
	}

	// Generate prime numbers and assign them to q, p. Then calculate phi.
	void genPrimes(int size) {
		this->genRandomArray(rsa.p, size);
		this->genRandomArray(rsa.q, size);

		// set prime P
		while (!this->isPrime(rsa.p)) {
			mpz_add_ui(rsa.p, rsa.p, 2);
		}
		// set prime Q
		while (!this->isPrime(rsa.q)) {
			mpz_add_ui(rsa.q, rsa.q, 2);
		}
	}

	void genPhi() {
		// calculate phi
		mpz_sub_ui(rsa.p, rsa.p, 1);
		mpz_sub_ui(rsa.q, rsa.q, 1);
		mpz_mul(rsa.phi, rsa.p, rsa.q);
		mpz_add_ui(rsa.p, rsa.p, 1);
		mpz_add_ui(rsa.q, rsa.q, 1);
	}

	void genN() {
		// calculate N public modulus
		mpz_mul(rsa.n, rsa.p, rsa.q);
	}

	void genGcd(mpz_t gcd, mpz_t n1, mpz_t n2) {
		// Copies of arguments
		mpz_t N1;
		mpz_init(N1);
		mpz_set(N1, n1);

		mpz_t N2;
		mpz_init(N2);
		mpz_set(N2, n2);

		mpz_t tmp;
		mpz_init(tmp);

		while (mpz_cmp_si(N2, 0)) {
			mpz_set(tmp, N1);
			mpz_set(N1, N2);
			mpz_mod(N2, tmp, N2);
		}

		mpz_set(gcd, N1);
		return;
	}

	// extended Euclid algorithm
	/* https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/ */
	// e phi
	void genMulInverse() {
		mpz_t e;
		mpz_init(e);
		mpz_set(e, rsa.e);

		mpz_t phi;
		mpz_init(phi);
		mpz_set(phi, rsa.phi);

		mpz_t y;
		mpz_init(y);
		mpz_set_ui(y, 0);

		mpz_t x;
		mpz_init(x);
		mpz_set_ui(x, 1);
		
		mpz_t q;
		mpz_init(q);

		mpz_t t;
		mpz_init(t);

		if (!mpz_cmp_si(phi, 1)) {
			mpz_set_si(rsa.d, 0);
			return;
		}

		while (mpz_cmp_si(e, 1) > 0) {
			mpz_div(q, e, phi);
			mpz_set(t, phi);

			mpz_mod(phi, e, phi);
			mpz_set(e, t);
			mpz_set(t, y);

			mpz_mul(y, q, y);
			mpz_sub(y, x, y);
			
			mpz_set(x, t);
		}

		if (mpz_cmp_si(x, 0) < 0) {
			mpz_add(x, x, rsa.phi);
		}
		mpz_set(rsa.d, x);

		return;
	}

	// gcd(phi(n),e)==1
	void genExp() {
		mpz_t gcd;
		mpz_init(gcd);
		do {
			this->genRandomArray(rsa.e, mpz_sizeinbase(rsa.phi, 2) / 8);
			this->genGcd(gcd, rsa.phi, rsa.e);
		} while(mpz_cmp_ui(gcd, 1));
	}

	/* calculates Jacobian(a/n) n>0 and n is odd  */
	void genJacobian(mpz_t jacobian, mpz_t a, mpz_t potentialPrime) {
		if (!mpz_cmp_si(a, 0)) {
			mpz_set_ui(jacobian, 0);
			return;
		}

		// copy of variable a
		mpz_t tmpA;
		mpz_init(tmpA);
		mpz_set(tmpA, a);

		// copy of variable pp
		mpz_t tmpPotentialPrime;
		mpz_init(tmpPotentialPrime);
		mpz_set(tmpPotentialPrime, potentialPrime);

		// temporary variable for arithmetic operations
		mpz_t aux;
		mpz_init(aux);

		// ans
		int ans = 1;

		if (mpz_cmp_si(a, 0) < 0) {
			mpz_mul_si(tmpA, tmpA, -1);
			mpz_mod_ui(aux, tmpPotentialPrime, 4);
			if (!mpz_cmp_si(aux, 3)) {
				ans*= -1;
			}
		}

		if (!mpz_cmp_si(tmpA, 1)) {
			mpz_set_si(jacobian, ans);
			return;
		}

		while (mpz_cmp_ui(tmpA, 0)) {
			if (mpz_cmp_ui(tmpA, 0) < 0) {
				mpz_mul_si(tmpA, tmpA, -1);

				mpz_mod_ui(aux, tmpPotentialPrime, 4);
				if (!mpz_cmp_si(aux, 3)) {
					ans*= -1;
				}
			}
			while (mpz_even_p(tmpA)) {
				mpz_div_ui(tmpA, tmpA, 2);
				mpz_mod_ui(aux, tmpPotentialPrime, 8);
				if (mpz_cmp_si(aux, 3) == 0 || mpz_cmp_si(aux, 5) == 0) {
					ans*= -1;   
				}
			}

			// swap
			mpz_set(aux, tmpPotentialPrime);
			mpz_set(tmpPotentialPrime, tmpA);
			mpz_set(tmpA, aux);

			mpz_mod_ui(aux, tmpA, 4);
			if (!mpz_cmp_si(aux, 3)) {
				mpz_mod_ui(aux, tmpPotentialPrime, 4);
				if (!mpz_cmp_si(aux, 3)) {
					ans*= -1;
				}
			}

			mpz_mod(tmpA, tmpA, tmpPotentialPrime);
			mpz_div_ui(aux, tmpPotentialPrime, 2);
			 if (mpz_cmp(tmpA, aux) > 0) {
				mpz_sub(tmpA, tmpA, tmpPotentialPrime);
			}
		}

		if (!mpz_cmp_si(tmpPotentialPrime, 1)) {
			mpz_set_si(jacobian, ans);
			return;
		}
		mpz_set_si(jacobian, 0);
		return;
	}

	/* Solovay-Strassen Primality Test */
	bool isPrime(mpz_t potentialPrime) {
		if(mpz_cmp_ui(potentialPrime, 2) < 0) {
			return false;
		}

		if(mpz_cmp_ui(potentialPrime, 2) != 0 && mpz_even_p(potentialPrime) != 0){
			return false;
		}

		mpz_t a;
		mpz_init(a);

		mpz_t mod;
		mpz_init(mod);

		mpz_t jacobian;
		mpz_init(jacobian);

		mpz_t ppMinusOne;
		mpz_init(ppMinusOne);
		
		for (int i = 0; i < 20; i++) {
			mpz_sub_ui(ppMinusOne, potentialPrime, 1);
			this->genRandomArray(a, 4);
			mpz_mod(a, a, ppMinusOne);
			mpz_add_ui(a, a, 1);

			this->genJacobian(jacobian, a, potentialPrime);

			mpz_add(jacobian, jacobian, potentialPrime);
			mpz_mod(jacobian, jacobian, potentialPrime);

			mpz_div_ui(ppMinusOne, ppMinusOne, 2);
			mpz_powm(mod, a, ppMinusOne, potentialPrime);

			if (!mpz_cmp_si(jacobian, 0) || mpz_cmp(mod, jacobian)) {
				return false;
			}
		}
		return true;
	}

public:
	RSA () {
		// Class Constructor
		mpz_init(rsa.c);
		mpz_init(rsa.d);
		mpz_init(rsa.m);
		mpz_init(rsa.n);
		mpz_init(rsa.p);
		mpz_init(rsa.q);
		mpz_init(rsa.e);
		mpz_init(rsa.phi);
		// Random state initialization
		srand(time(NULL));
		mpz_init(rsa.seed);
		gmp_randinit_default(random_state);
		this->genRandomArray(rsa.seed, 10);
		gmp_randseed(random_state, rsa.seed);
	}

	// Function for encryption/decryption (both -e and -d flags)
	void cipher_or_decipher(char *exp, char *n, char *str) {
		mpz_init_set_str(rsa.m, str+2, 16);
		mpz_init_set_str(rsa.e, exp+2, 16);
		mpz_init_set_str(rsa.n, n+2, 16);
		mpz_powm(rsa.c, rsa.m, rsa.e, rsa.n);
		gmp_printf("0x%Zx\n", rsa.c);
	}

	// Function to generate RSA parameters
	void generate(char *b) {
		int primesSize = atoi(b) / 16;
		if(primesSize < 1) {
			exit(-1);
		}

		// Generate P, Q primes
		this->genPrimes(primesSize);
		// Calculate PHI
		this->genPhi();
		// Calculate N public modulus
		this->genN();
		// Calculate Exponent
		this->genExp();
		// Calculate d
		this->genMulInverse();

		gmp_printf("0x%Zx 0x%Zx 0x%Zx 0x%Zx 0x%Zx\n", rsa.p, rsa.q, rsa.n, rsa.e, rsa.d);
	}

	void crack(char *b) {
		/* TO DO */
		return;
	}
	
};

// Function to display error
int throwError(string help) {
	cerr << help;
	return -1;
}


/*
	M*A*I*N
*/
int main(int argc, char** argv) {
	// Const help string
	string help = (
		"./kry\n"
		"        -g B     (Generování klíčů)\n"
		"        -e E N M (Šifrování)\n"
		"        -d D N C (Dešifrování)\n"
		"        -b N     (Prolomení RSA)\n"
		"B...požadovaná velikost veřejného modulu v bitech (např. 1024)\n"
		"P...prvočíslo (při generování náhodné)\n"
		"Q...jiné prvočíslo (při generování náhodné)\n"
		"N...veřejný modulus\n"
		"E...veřejný exponent (většinou 3)\n"
		"D...soukromý exponent\n"
		"M...otevřená zpráva (číslo, nikoli text)\n"
		"C...zašifrovaná zpráva (číslo, nikoli text)\n"
	);

	// Handling arguments
	if(argc == 3) {
		if(strcmp(argv[1], "-g") == 0){
			RSA rsa;
			rsa.generate(argv[2]);
			return 0;
		}else if(strcmp(argv[1], "-b") == 0){
			RSA rsa;
			rsa.crack(argv[2]);
		}else{
			throwError(help);
		}
	}
	else if(argc == 5){
		if(strcmp(argv[1], "-e") == 0){
			RSA rsa;
			rsa.cipher_or_decipher(argv[2], argv[3], argv[4]);
			return 0;
		}else if(strcmp(argv[1], "-d") == 0){
			RSA rsa;
			rsa.cipher_or_decipher(argv[2], argv[3], argv[4]);
			return 0;
		}else{
			throwError(help);
		}
	}else{
		throwError(help);
	}

	return 0;
}