help:
        @echo "make brent - build optimized version"
        @echo "make debug - build DEBUG version"
        @echo "make trdy  - run valency lexicon extraction test"
        @echo "make clean - remove binary and temporary files"

brent: cue.y
        yacc -d cue.y
        mkdir -p ../bin
        gcc -O2 -o ../bin/brent y.tab.c -lm
        strip ../bin/brent

debug: cue.y
        yacc -d cue.y
        mkdir -p ../bin
        gcc -g -DDEBUG -o ../bin/brent y.tab.c -lm

test: ../bin/brent
        time ../bin/brent ../lib/corpus > ../lib/verb-valency-lexicon.lex

clean:
        rm -f *~ *.o y.tab.c y.tab.h ../bin/brent
