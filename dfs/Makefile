#

.SILENT:

all: dodeca

dodeca: dodeca.adb dodeca-*.ad? dfs.ad?
	gnatmake -O dodeca

clean:
	rm -f *.o *.ali dodeca
