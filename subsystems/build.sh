#/bin/sh

. ./config.sh

for ss in $SUBSYSTEMS; do
    cd $ss

    echo; echo Building subsystem $ss...
    time make clobber build tests

    cd ..
done

exit

# vim:sw=4:ts=4:et
