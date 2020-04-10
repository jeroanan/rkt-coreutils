RACO=raco

DUTIL=src/util
DLIBC=src/libc

all: compiled/rkt-ls compiled/rkt-echo compiled/rkt-stat compiled/rkt-head

compiled/rkt-ls: src/ls.rkt $(DUTIL)/human-size.rkt $(DUTIL)/human-date.rkt $(DUTIL)/fileaccessstr.rkt $(DLIBC)/stat.rkt $(DLIBC)/pwd.rkt $(DLIBC)/grp.rkt 
	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

compiled/rkt-echo: src/echo.rkt
	$(RACO) exe -o compiled/rkt-echo src/echo.rkt

compiled/rkt-stat: src/stat.rkt $(DLIBC)/stat.rkt $(DLIBC)/pwd.rkt $(DLIBC)/grp.rkt $(DUTIL)/fileaccessstr.rkt $(DUTIL)/fileaccessoct.rkt
	$(RACO) exe -o compiled/rkt-stat src/stat.rkt

compiled/rkt-head: src/head.rkt
	$(RACO) exe -o compiled/rkt-head src/head.rkt

clean:
	rm -rf compiled

$(shell mkdir -p compiled)
