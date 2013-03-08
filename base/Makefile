all:
	for foo in `git grep -l 'lang plai-typed$$' | grep '\.rkt$$'`; do sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g $$foo; done && git ls-files  | grep '\.rkt$$' | xargs raco make -j 5

typecheck:
	for foo in `git ls-files | grep '\.rkt$$'`; do `which cp` -p $$foo $$foo"_"; done && git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g && git ls-files | grep '\.rkt$$' | xargs raco make -j 5; for foo in `git ls-files | grep '\.rkt$$'`; do `which cp` -p $$foo"_" $$foo && rm $$foo"_"; done

typed:
	for foo in `git grep -l 'lang plai-typed/untyped' | grep '\.rkt$$'`; do  sed -i 's/lang plai-typed\/untyped/lang plai-typed/g' $$foo; done && raco make python-main.rkt

test:
	racket python-main.rkt --python-path `which python3.2` --test ../tests/python-reference
