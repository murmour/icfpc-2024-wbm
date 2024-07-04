
interact: kepler.mli kepler.ml interact.ml
	ocamlfind opt -linkpkg -package 'curl' $^ -o $@

MageML: kepler.mli kepler.ml CharStream.mli CharStream.ml peg_MageML.mli peg_MageML.ml MageML.ml
	ocamlfind opt -linkpkg -package 'zarith' $^ -o $@

clean:
	rm -rf *.o *.cmi *.cmx kepler interact MageML
