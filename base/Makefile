MOD := $(shell for foo in $$(git diff --name-only | grep '\.rkt$$' | sed s%.*/%%g | grep -v 'main.rkt'); do find . -name $$foo; done)
FILES := "${MOD}"
all:
	for foo in `git grep -l 'lang plai-typed$$' | grep '\.rkt$$'`; do sed -i s/'lang plai-typed$$'/'lang plai-typed\/untyped'/g $$foo; done && git ls-files  | grep '\.rkt$$' | xargs raco make -j 5

typecheck:
	@echo $(FILES) && for foo in `git ls-files | grep '\.rkt$$'`; do `which cp` -p $$foo $$foo"_"; done && git ls-files | grep '\.rkt$$' | xargs sed -i s/'lang plai-typed\/untyped'/'lang plai-typed'/g && if [[ $(FILES) ]]; then echo $(FILES) | xargs raco make -j 5 && make clean; else echo "nothing to build"; fi; for foo in `git ls-files | grep '\.rkt$$'`; do `which cp` -p $$foo"_" $$foo && rm $$foo"_"; done 

typed:
	for foo in `git grep -l 'lang plai-typed/untyped' | grep '\.rkt$$'`; do  sed -i 's/lang plai-typed\/untyped/lang plai-typed/g' $$foo; done && for foo in `git ls-files | grep '\.rkt$$'`; do raco make $$foo; done

test:
	racket python-main.rkt --python-path `which python3.2` --test ../tests/python-reference
clean:
	@`which rm` compiled/*
