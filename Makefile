minilisp: minilisp.ml
	ocamlfind c -pp 'camlp4o' $< -o $@

