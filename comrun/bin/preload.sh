#!/bin/bash

# TODO get env dynamically
CODECHECK_HOME=/opt/codecheck
MAXOUTPUTLEN=10000

BASE=$(pwd)
PATH=$PATH:/usr/lib/kotlinc/bin

# args: dir sourceDir sourceDir ...
function prepare {
  cd $BASE
  mkdir $1
  cd $1
  shift
  for d in $@ ; do cp -R $BASE/$d/* . 2>/dev/null ; done  
}

# args: dir language sourcefiles
function compile {
  DIR=$1
  shift  
  LANG=$1
  shift
  cd $BASE/$DIR
  mkdir -p $BASE/out/$DIR  
  case _"$LANG" in 
    _C)
      gcc -std=c99 -g -o prog $@ -lm > $BASE/out/$DIR/_compile 2>&1
      ;;
    _Cpp)
      g++ -std=c++17 -Wall -Wno-sign-compare -g -o prog $@ 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _CSharp)
      mcs -o Prog.exe $@  > $BASE/out/$DIR/_compile 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _Dart)
      dart --disable-analytics compile exe -o prog $@ > $BASE/out/$DIR/_compile 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _Haskell)
      ghc -o prog $@ > $BASE/out/$DIR/_compile 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _Java)
      javac -cp .:$BASE/use/\* $@ > $BASE/out/$DIR/_compile 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _Bash|_JavaScript|_Matlab)
      touch $BASE/out/$DIR/_compile
      ;;
    _Racket)
      echo Racket > $BASE/out/$DIR/_compile      
      ;;
    _Python)
      python3 -m py_compile $@ 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _Scala)
      scalac $@ 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile   
      ;;
    _Kotlin)
       kotlinc $@ 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile   
       ;;
    _SML)
      polyc -o prog $1 > $BASE/out/$DIR/_compile 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_compile
      ;;
    _Rust)
      rustc -g -o prog $1 > $BASE/out/$DIR/_compile 2>&1
      ;;
    *)  
      echo Unknown language $LANG > $BASE/out/$DIR/_errors 
      ;;                      
  esac 
  if [[ ${PIPESTATUS[0]} != 0 ]] ; then
    mv $BASE/out/$DIR/_compile $BASE/out/$DIR/_errors
    find -name "*.class" -exec rm {} \;
  fi  
}

# args: dir id timeout interleaveio language module arg1 arg2 ...
function run {
  DIR=$1
  shift
  ID=$1
  shift
  TIMEOUT=$1
  shift
  MAXOUTPUTLEN=$1
  shift  
  INTERLEAVEIO=$1
  shift  
  LANG=$1
  shift  
  MAIN=$1
  shift
  cd $BASE/$DIR
  mkdir -p $BASE/out/$ID
  case _"$LANG" in 
    _C|_Cpp|_Dart|_Haskell|_Rust)
      ulimit -d 100000 -f 1000 -n 100 -v 100000
      if [[ -e prog ]] ; then
        if [[ $INTERLEAVEIO == "true" ]] ; then
           timeout -v -s 9 ${TIMEOUT}s ${CODECHECK_HOME}/interleaveio.py ./prog $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
        else 
           timeout -v -s 9 ${TIMEOUT}s ./prog $@ < $BASE/in/$ID > $BASE/out/$ID/_run 2>&1
        fi
      fi
      ;;
    _SML)
      ulimit -d 1000000 -f 1000 -n 100 -v 1000000
      if [[ -e prog ]] ; then
        if [[ $INTERLEAVEIO == "true" ]] ; then
           timeout -v -s 9 ${TIMEOUT}s ${CODECHECK_HOME}/interleaveio.py ./prog $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
        else 
           timeout -v -s 9 ${TIMEOUT}s ./prog $@ < $BASE/in/$ID > $BASE/out/$ID/_run 2>&1
        fi
      fi
      ;;
    _Java)
      ulimit -d 1000000 -f 1000 -n 100 -v 10000000
      if [[ -e  ${MAIN/.java/.class} ]] ; then
        if [[ $INTERLEAVEIO == "true" ]] ; then
          timeout -v -s 9 ${TIMEOUT}s ${CODECHECK_HOME}/interleaveio.py java -ea -Djava.awt.headless=true -Dcom.horstmann.codecheck -cp .:$BASE/use/\* ${MAIN/.java/} $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
          cat hs_err*log >> $BASE/out/$ID/_run 2> /dev/null
          rm -f hs_err*log
        else
          timeout -v -s 9 ${TIMEOUT}s java -ea -Djava.awt.headless=true -Dcom.horstmann.codecheck -cp .:$BASE/use/\* ${MAIN/.java/} $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
          cat hs_err*log >> $BASE/out/$ID/_run 2> /dev/null
          rm -f hs_err*log          
        fi
      fi
      ;;
    _Bash)
      ulimit -d 10000 -f 1000 -n 100 -v 100000 
      if [[ -e premain.sh ]] ; then    
        TMPFILE=$(mktemp)
        echo "chmod -r main.sh" >> $TMPFILE
        cat premain.sh >> $TMPFILE
        echo -e "\n" >>  $TMPFILE
        cat $MAIN >> $TMPFILE
        rm premain.sh
        mv $TMPFILE $MAIN
      fi
      chmod +x *.sh
      timeout -v -s 9 ${TIMEOUT}s bash $MAIN $@  < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN >> $BASE/out/$ID/_run
      cat $BASE/out/$ID/_run
      ;;
    _CSharp)
      ulimit -d 10000 -f 1000 -n 100 -v 100000 
      if [[ -e Prog.exe ]] ; then    
        timeout -v -s 9 ${TIMEOUT}s mono Prog.exe $@  < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
      fi
      ;;
    _JavaScript)
      # sed -i -e 's/^const //g' *CodeCheck.js # TODO Horrible hack for ancient node version--remove
      # TODO Check if still nodejs or node with Ubuntu 20.04
      ulimit -d 100000 -f 1000 -n 100 -v 1000000      
      timeout -v -s 9 ${TIMEOUT}s node $MAIN $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run    
      ;;
    _Matlab)
      ulimit -d 10000 -f 1000 -n 100 -v 1000000
      NO_AT_BRIDGE=1 timeout -v -s 9 ${TIMEOUT}s octave --no-gui $MAIN $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run    
      ;;
    _Python)
      ulimit -d 100000 -f 1000 -n 100 -v 100000
      export CODECHECK=true
      if [[ -n $BASE/out/$DIR/_errors ]] ; then
        if [[ $INTERLEAVEIO == "true" ]] ; then
           timeout -v -s 9 ${TIMEOUT}s ${CODECHECK_HOME}/interleaveio.py python3 $MAIN $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
        else 
           timeout -v -s 9 ${TIMEOUT}s python3 $MAIN $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
        fi
      fi
      ;;
    _Racket)
      ulimit -d 1000000 -f 1000 -n 100 -v 1000000
      if grep -qE '\(define\s+\(\s*main\s+' $MAIN ; then
        timeout -v -s 9 ${TIMEOUT}s racket -tm $MAIN $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN >> $BASE/out/$ID/_run
      else
        timeout -v -s 9 ${TIMEOUT}s racket -t $MAIN $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN >> $BASE/out/$ID/_run
      fi    
      ;;
    _Scala)
      ulimit -d 1000000 -f 1000 -n 100 -v 10000000
      timeout -v -s 9 ${TIMEOUT}s scala ${MAIN/.scala/} $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
      ;;
    _Kotlin)
      ulimit -d 1000000 -f 1000 -n 100 -v 10000000
      timeout -v -s 9 ${TIMEOUT}s kotlin ${MAIN/.kt/Kt} $@ < $BASE/in/$ID 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$ID/_run
      ;;
    *)  
      echo Unknown language $LANG > $BASE/out/$ID/_run 
      ;;                
  esac 
}

# args: dir timeout lang mainsource source2 source3 ... 
function unittest {
  DIR=$1
  shift
  TIMEOUT=$1
  shift  
  LANG=$1
  shift  
  MAIN=$1
  shift
  mkdir -p $BASE/$DIR
  mkdir -p $BASE/out/$DIR
  cd $BASE/$DIR
  case _"$LANG" in 
    _Java)
      javac -cp .:$BASE/use/\*:$CODECHECK_HOME/lib/\* $MAIN $@ 2>&1 | head --lines $MAXOUTPUTLEN> $BASE/out/$DIR/_compile
      if [[ ${PIPESTATUS[0]} != 0 ]] ; then
        mv $BASE/out/$DIR/_compile $BASE/out/$DIR/_errors
      else
        ulimit -d 1000000 -f 1000 -n 100 -v 10000000
        timeout -v -s 9 ${TIMEOUT}s java -ea -Djava.awt.headless=true -cp .:$BASE/use/\*:$CODECHECK_HOME/lib/\* org.junit.runner.JUnitCore ${MAIN/.java/} 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_run
      fi
      ;;
    _Python)
      ulimit -d 100000 -f 1000 -n 100 -v 100000      
      timeout -v -s 9 ${TIMEOUT}s python3 -m unittest $MAIN 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_run    
      ;;
    _Racket)
      ulimit -d 100000 -f 1000 -n 100 -v 1000000       
      timeout -v -s 9 ${TIMEOUT}s racket $MAIN 2>&1 | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_run
      ;;
    _Rust)
      rustc -o prog --test $MAIN >> $BASE/out/$DIR/_compile
      if [[ ${PIPESTATUS[0]} != 0 ]] ; then
        mv $BASE/out/$DIR/_compile $BASE/out/$DIR/_errors
      else
        ulimit -d 100000 -f 1000 -n 100 -v 100000
        timeout -v -s 9 ${TIMEOUT}s ./prog | head --lines $MAXOUTPUTLEN > $BASE/out/$DIR/_run
      fi
      ;;
  esac     
}

# args: dir command args
function process {
  DIR=$1
  shift
  CMD=$1
  shift
  ARGS=$@
  cd $BASE/$DIR
  mkdir -p $BASE/out/$DIR
  case _"$CMD" in 
    _CheckStyle)
      java -cp $CODECHECK_HOME/lib/\* com.puppycrawl.tools.checkstyle.Main -c checkstyle.xml $ARGS > $BASE/out/$DIR/_run 2>&1
    ;;
  esac 
}

# args: dir file1 file2 ...
function collect {
  DIR=$1
  shift
  cd $BASE/$DIR
  cp --parents $@ $BASE/out/$DIR
}
