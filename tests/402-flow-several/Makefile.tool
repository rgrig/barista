METHOD1=java.lang.Integer.toUnsignedString(int,int):java.lang.String
METHOD2=java.lang.Class.getSimpleBinaryName():java.lang.String

default: run diff clean

run: FORCE
	@$(EXEC) flow -cp $(CLASSPATH) '$(METHOD1)' '$(METHOD2)' > result

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -f result

FORCE:
