RACO=raco
SCRIBBLE=scribble

DUTIL=src/util
DLIBC=src/libc
VERSIONFILE=src/util/version.rkt

AUTHSCRBL=scribbles/author.scrbl
CWSCRBL = scribbles/copyright.scrbl

all: compiled/rkt-ls compiled/rkt-echo compiled/rkt-stat compiled/rkt-head docs

compiled/rkt-ls: src/ls.rkt $(DUTIL)/human-size.rkt $(DUTIL)/human-date.rkt $(DUTIL)/fileaccessstr.rkt $(DLIBC)/stat.rkt $(DLIBC)/pwd.rkt $(DLIBC)/grp.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

compiled/rkt-echo: src/echo.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-echo src/echo.rkt

compiled/rkt-stat: src/stat.rkt $(DLIBC)/stat.rkt $(DLIBC)/pwd.rkt $(DLIBC)/grp.rkt $(DUTIL)/fileaccessstr.rkt $(DUTIL)/fileaccessoct.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-stat src/stat.rkt

compiled/rkt-head: src/head.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-head src/head.rkt

docs: docs/html/ls.html docs/html/echo.html

docs/html/ls.html: scribbles/ls.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/ls.scrbl

docs/html/echo.html: scribbles/echo.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/echo.scrbl

clean:
	rm -rf compiled
	rm -rf docs/

$(shell mkdir -p compiled)
