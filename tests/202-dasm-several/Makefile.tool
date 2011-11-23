default: run diff clean

run: FORCE
	@$(EXEC) dasm -cp $(CLASSPATH) -cp ../classes pack.TestGen pack.TestAnnot pack.TestFrame java.lang.Throwable > result

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -f result

FORCE:
