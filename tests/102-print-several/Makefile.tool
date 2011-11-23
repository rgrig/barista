default: run diff clean

run: FORCE
	@$(EXEC) print -cp $(CLASSPATH) -cp ../classes pack.TestGen pack.TestAnnot > result

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -f result

FORCE:
