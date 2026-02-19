#/bin/sh

SUBSYSTEMS="control generics music terminal"

ALLINONE=liballinone.a

function build {
    for ss in $SUBSYSTEMS; do
        cd $ss
        echo; echo Building subsystem $ss...
        time make clobber build tests
        cd ..
    done
}

build
./merge.sh

exit

# vim:sw=4:ts=4:et
