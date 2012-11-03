TARGET=minilisp
TEST=test
SRCS=expr.ml parser.mli parser.ml env.ml eval.mli eval.ml 

${TARGET}: ${SRCS} minilisp.ml
	ocamlfind c -g -pp 'camlp4o' $? -o $@

${TEST}: ${SRCS} test.ml
	ocamlfind c -g -pp 'camlp4o' $? -o $@
	./${TEST}

clean:
	rm -f ${TARGET} ${TEST} *.cm*

