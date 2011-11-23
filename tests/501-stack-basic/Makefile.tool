default: run diff clean

run: FORCE
	@$(JAVAC) -cp $(CLASSPATH) Source.java -d .
	@$(EXEC) dasm -cp $(CLASSPATH) pack.Source | grep -v '  .frame' | grep -v '  .max_stack' | grep -v '  .max_locals' > tmp.j
	@rm -fr pack
	@$(EXEC) asm -cp $(CLASSPATH) -compute-stacks tmp.j > /dev/null
	@$(EXEC) dasm -cp $(CLASSPATH) pack.Source > result
	@$(JAVA) -cp $(CLASSPATH) -Xverify:all -XX:-FailOverToOldVerifier pack.Source

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -fr pack tmp.j result

FORCE:
