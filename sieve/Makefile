#

.SILENT:

all: sieve

sieve: sieve.adb
	gnatmake -q -gnat2020 sieve

clean:
	rm -f *.o *.ali sieve *.ads.* *.adb.*
