consensus
=========

Solution of the Byzantine Generals problem. Protocol: Phase King. Implemented in: Erlang & C

Running simulation under Erlang
-------------------------------
A basic Erlang installation is required. Compile and run with

    $ cd erlang
    $ erlc simul.erl
    $ erl -noshell -s simul game -s init stop < ../tests/tra0.txt

Colours tested under Bash and Zsh.

Running simulation under C
--------------------------
Compile and run.

    $ cd c
    $ make
    $ ./simul < ../tests/tra0.txt
