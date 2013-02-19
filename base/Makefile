all:
	for foo in `git grep 'lang plai-typed$$' | grep '\.rkt$$'`; do sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g $foo; done && git ls-files  | grep '\.rkt$$' | xargs raco make -j 5

typecheck:
	git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g && git ls-files | grep '\.rkt$$' | xargs raco make -j 5; git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g

typed:
	for foo in `git grep 'lang plai-typed/untyped' | grep '\.rkt$$'`; do sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g $foo; done && git ls-files | grep '\.rkt$$' | xargs raco make -j 5
