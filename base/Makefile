MOD := $(shell for foo in $$(git diff --name-only | grep '\.rkt$$' | sed s%.*/%%g | grep -v 'main.rkt'); do find . -name $$foo; done)
FILES := "${MOD}"
all:
	for foo in `git grep -l 'lang plai-typed$$' | grep '\.rkt$$'`; do sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g $$foo; done && git ls-files  | grep '\.rkt$$' | xargs raco make -j 5 && grep -r -l  --color='auto' -P -n "[\x80-\xFF]" | sed s/^/"Warning: found non-ascii characters: "/g | grep -v pyc$$ | grep -v zo$$ | grep -v rkt$$; make clean-python; true

typecheck:
	@echo $(FILES) && for foo in `git ls-files | grep '\.rkt$$'`; do `which cp` -p $$foo $$foo"_"; done && git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g && if [[ $(FILES) ]]; then echo $(FILES) | xargs raco make -j 5 && make clean; else echo "nothing to build"; fi; for foo in `git ls-files | grep '\.rkt$$'`; do `which cp` -p $$foo"_" $$foo && rm $$foo"_"; done 

typed:
	for foo in `git grep -l 'lang plai-typed/untyped' | grep '\.rkt$$'`; do  sed -i 's/lang plai-typed\/untyped/lang plai-typed/g' $$foo; done && for foo in `git ls-files | grep '\.rkt$$'`; do raco make -j 5 $$foo; done

test:
	racket python-main.rkt --python-path `which python3.2` --test ../tests/python-reference

cpstest:
	raco make cps-test.rkt
	racket python-main.rkt --python-path `which python3.2` --test-cps
	@echo "cps tests done."

clean:
	@`which rm` compiled/*

clean-python:
	@touch foo.pyc && touch __pycache__ && find . -iname '*.pyc' | xargs `which rm` && find . -name __pycache__ | xargs `which rm` 
