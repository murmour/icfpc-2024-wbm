
kepler: kepler.mli kepler.ml
	ocamlopt $^ -o kepler

interact: kepler.mli kepler.ml interact.ml
	ocamlfind opt -linkpkg -package 'curl' $^ -o interact

clean:
	rm -rf *.o *.cmi *.cmx kepler interact
