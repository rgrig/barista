default: compile run diff clean

compile: FORCE
	@$(EXEC) asm -optimize source.j > /dev/null

run: FORCE
	@$(JAVAP) -c pack.Test > result

diff: FORCE
	@diff -q result reference

clean: FORCE
	@rm -f result
	@rm -fr pack

FORCE:
