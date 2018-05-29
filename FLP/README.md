# Funkcionální a logické programování 2016L

### Fakulta informačních technologií
### Vysoké Učení Technické

## Dokumentace k funkcionálnímu projektu
- varianta: bkg-2-cnf

1. Spracovanie argumentov
	
	Na spracovanie argumentov prikazovej riadky som pouzil kostru prebratu z laboratorneho cvicenia predmetu FLP.
	V samostatnej knihovni obsahujucej definovany datovy typ Config ktory po uspesnom vyhodnoteni vstupnych argumentov obsahuje zdroj vstupnej gramatiky (subor/stdin) a akciu, ktora sa ziada vykonat.

2. Parsovanie vstupnej gramatiky
	
	Parsovanie vstupnej gramatiky prebieha za pomoci parsera ReadP. Riesenie je rovnako inspirovane pocitacovym cvicenim.
	Parsovacia funkcia rovnako ako definovane datove struktury pre ulozenie gramatiky sa nachadzaju v samostatnom subore UTM.hs

3. Funkcionalita

	V projekte bola uspesne implementovana funkcionalita vsetkych troch prepinacov.

4. Automatizovane testy

	Prilozene v projekte su aj vlastne automatizovane testy overujuce funkcionalitu. 
	Spustenie testovacieho skriptu ./bkg2cnf-tests.sh

5. Vyvoj & testovanie

	Projekt vyvijany na OS Ubuntu 16.04 LTS s prekladacom GHC 7.10.3.
	Projekt bol otestovany na referencnom stroji Merlin.
