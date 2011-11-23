default:
	@for file in `ls *.j`; do \
	    $(EXEC) asm -compute-stacks $$file > $$file.result 2>&1; \
	    diff -q $$file.result $$file.reference || exit 1; \
	done
	@rm -f *.result
