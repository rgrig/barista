default: run diff clean

run: FORCE
	@$(EXEC) dasm -cp $(CLASSPATH) java.lang.Object > result

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -f result

FORCE:
