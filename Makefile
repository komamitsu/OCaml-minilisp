minilisp: minilisp.ml
	ocamlfind c -g -pp 'camlp4o' $< -o $@

