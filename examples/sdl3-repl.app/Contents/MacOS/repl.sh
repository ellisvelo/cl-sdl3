#!/bin/sh 

if [ "${OS}" != "Windows_NT" ]; then
    LIB_PATH="$HOME/local/lib/"
else
    LIB_PATH=`cygpath -m $HOME/local/lib/`
fi    

sbcl --dynamic-space-size 2048 \
     --eval "(asdf:load-system :cffi :verbose nil)" \
     --eval "(pushnew \"${LIB_PATH}\" cffi:*foreign-library-directories* :test #'equal)" \
     --eval "(pushnew \"/usr/local/lib/\" cffi:*foreign-library-directories* :test #'equal)" \
     --eval "(asdf:load-system :sdl3-examples :verbose nil)" \
     --eval "(setf asdf:*compile-file-failure-behaviour* :warn)" \
     --eval "(asdf:load-system :swank :verbose nil)" \
     --eval "(setf asdf:*compile-file-failure-behaviour* :error)" \
     --eval "(bt:make-thread (lambda () (swank:create-server :port 4005 :dont-close t)))"

