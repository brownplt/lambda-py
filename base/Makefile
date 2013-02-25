all:
	for foo in `git grep -l 'lang plai-typed$$' | grep '\.rkt$$'`; do cp $$foo $$foo"_"; touch -r $$foo $$foo"_"; sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g $$foo; touch -r $$foo"_" $$foo; rm $$foo"_"; done && git ls-files  | grep '\.rkt$$' | xargs raco make -j 5

typecheck:
	git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g && git ls-files | grep '\.rkt$$' | xargs raco make -j 5; git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g

typed:
	for foo in `git grep -l 'lang plai-typed/untyped' | grep '\.rkt$$'`; do cp $$foo $$foo"_"; touch -r $$foo $$foo"_"; sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g $$foo; touch -r $$foo"_" $$foo; rm $$foo"_"; done && git ls-files | grep '\.rkt$$' | xargs raco make -j 5
