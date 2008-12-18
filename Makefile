.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

MODS = hexutil

all: compile

compile: ${MODS:%=%.beam}

test: ${MODS:%=%.beam}
	${ERL} -s ${<:%.beam=%} test -s init stop

clean:	
	rm -rf *.beam erl_crash.dump
