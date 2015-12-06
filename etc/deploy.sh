#!/bin/sh

ssh rep 'for i in ~/quicklisp/local-projects/*(/); (cd $i; git pull)'
ssh rep 'sbcl --load ~/quicklisp/local-projects/letter/save-lisp-and-die.lisp'
ssh rep 'sudo service letter restart'

