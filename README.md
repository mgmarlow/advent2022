# Advent of Code 2022

[Advent of code 2022](https://adventofcode.com/) with Common lisp.

## Install

Install [SBCL](http://www.sbcl.org/):

```
brew install sbcl
```

Install [Quicklisp](https://www.quicklisp.org/beta/):

```
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
     --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit
```

Install ruby dependencies (if running rake task for benchmarks):

```
bundle
```

## Run

> Note: benchmarks aren't super accurate, especially for exercises that load quicklisp libraries.

Print answers for each day with benchmarks:

```
bin/rake benchmark
```
