# POS (Pokročilé operační systémy)

### Projekt 1 8b/8b

- Vytvořte program, který poběží celkem ve 3 procesech (spustí 2 nové), které jsou ve vztahu praotec -> otec -> syn.
- Každý z nich před spuštěním dalšího procesu vypíše na standardní výstup údaje o sobě v následujícím formátu:

	printf("%s identification: \n", label); /*grandparent/parent/child */
	printf("	pid = %d,	ppid = %d,	pgrp = %d\n", ...);
	printf("	uid = %d,	gid = %d\n", ...);
	printf("	euid = %d,	egid = %d\n", ...);

- Po spuštění následníka čeká každý proces na ukončení následníka a vypíše důvod ukončení:
	printf("%s exit (pid = %d):", label, ...); /* and one line from */
	printf("	normal termination (exit code = %d)\n", ...); /* or */
	printf("	signal termination %s(signal = %d)\n", ...); /* or */
	printf("	unknown type of termination\n");

- Maska "%s" v řádku informujícím o přerušení programu signálem má být nahrazena buď prázdným řetězcem "" nebo řetězcem "with core dump ". Pro testování důvodu ukončení použijte makra WIFEXITED(), WEXITSTATUS(), WIFSIGNALED(), WCOREDUMP(), WTERMSIG(). Syn (třetí proces) po výpisu informací o sobě nahradí sebe sama programem (funkce execv()), jehož absolutní jméno je uvedeno na příkazovém řádku jako první parametr. Takto spouštěný proces může mít další parametry, které se získají také z příkazového řádku(libovolný počet - a není nutné nikam nic kopírovat).

- Jako základ pro tento projekt použijte base.tar.gz. Při překladu kompilátorem gcc povinně zachovejte parametr -Wall a zachovejte i definici makra _XOPEN_SOURCE (a _XOPEN_SOURCE_EXTENDED kvůli WCOREDUMP() na Solarisu). Makro WCOREDUMP nemusí být na všech systéemech dostupné, proto bude jeho použití podmíněno jeho existencí (#ifdef WCOREDUMP .... #endif)
