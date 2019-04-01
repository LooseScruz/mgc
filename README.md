# mgc

To build:

1. sudo apt install opam
2. opam init
3. opam config env
4. opam install llvm
5. make


Notes:

String:
	| '"' [^ '"']* '"' as lxm { STRINGCON (lxm)  }
