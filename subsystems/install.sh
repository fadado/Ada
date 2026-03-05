#/bin/sh

SUBSYSTEMS="control generics music terminal"

ALLINONE=liballinone.a

LIB_DIR=./LIBRARY
INC_DIR=./INCLUDE

function merge {
    echo; echo Merging libraries...

    # Target directories
    [[ -d $LIB_DIR ]] || mkdir $LIB_DIR
    rm -f $LIB_DIR/*

    [[ -d $INC_DIR ]] || mkdir $INC_DIR
    rm -f $INC_DIR/*

    # Archive library
    ar -M <<-EOF
        CREATE $LIB_DIR/$ALLINONE
        $(for ss in $SUBSYSTEMS; do echo ADDLIB $ss/$LIB_DIR/lib$ss.a; done)
        SAVE
        END
EOF

    chmod 444 $LIB_DIR/$ALLINONE
}

function import {
    echo; echo Importing includes...

    # ALI files
    for ss in $SUBSYSTEMS; do
        cp --no-clobber $ss/$LIB_DIR/*.ali $LIB_DIR
    done

    chmod 444 $LIB_DIR/*.ali

    # *.ad? files
    for ss in $SUBSYSTEMS; do
        cp --no-clobber $ss/*.ad[bs] $INC_DIR
    done

    rm -f $INC_DIR/zilch_*

    chmod 444 $INC_DIR/*.ad[bs]
}

time merge
time import

exit

# vim:sw=4:ts=4:et
