#/bin/sh

PROGRAMS="sieve queens series"

ALLINONE=liballinone.a

function build {
    for pp in $PROGRAMS; do
        cd $pp
        echo; echo Building program $pp...
        time make clobber build
        cd ..
    done
}

build

exit

# vim:sw=4:ts=4:et
