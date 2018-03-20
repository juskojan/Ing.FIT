#!/bin/sh

if [ $# -eq 1 ];then
	numbers=$1
else
	echo "Pouziti: ./test.sh pocet_hodnot" >&2
    exit -1
fi

if [ $numbers -lt 1 ]; then
	echo "Prilis maly pocet!" >&2
fi

#preklad cpp zdrojaku
mpic++ --prefix /usr/local/share/OpenMPI -o es es.cpp

#vyrobeni souboru s random cisly
dd if=/dev/random bs=1 count=$numbers of=numbers 2>/dev/null

numbers=$((numbers+1))

#spusteni
mpirun --prefix /usr/local/share/OpenMPI -np $numbers es

#uklid
rm -rf es numbers