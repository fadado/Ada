#/bin/sh

SUBSYSTEMS="control generics music terminal"

ALLINONE=liballinone.a

function merge {
    echo; echo Merging subsystem libraries...

    [[ -d ./LIBRARY ]] || mkdir ./LIBRARY
    rm -f ./LIBRARY/*

    ar -M <<-EOF
        CREATE ./LIBRARY/$ALLINONE
        $(for ss in $SUBSYSTEMS; do echo ADDLIB $ss/LIBRARY/lib$ss.a; done)
        SAVE
        END
EOF

    chmod 444 ./LIBRARY/$ALLINONE

    for ss in $SUBSYSTEMS; do
        cp $ss/LIBRARY/*.ali ./LIBRARY
    done
}

time merge

exit

# vim:sw=4:ts=4:et
