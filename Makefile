all: compile

compile:
	emacs -Q --batch --eval '(byte-compile-file "sackspace.el")' 
