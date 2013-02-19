all:
	git ls-files | grep '\.rkt$$' | xargs raco make -j 5
typed:
	git diff --name-only | sed s%^%"`git rev-parse --show-toplevel`/"%g | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g && git ls-files | grep '\.rkt$$' | xargs raco make -j 5; git diff --name-only | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g
