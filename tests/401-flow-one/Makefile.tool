METHOD=java.lang.Integer.toUnsignedString(int,int):java.lang.String

default: run diff clean

run: FORCE
	@$(EXEC) flow -cp $(CLASSPATH) '$(METHOD)' > result

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -f result

FORCE:
