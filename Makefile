RACO=raco
SCRIBBLE=scribble

DUTIL=src/util
DLIBC=src/libc
VERSIONFILE=src/util/version.rkt
TYPEDEFS=src/typedef
REPL=src/repl

AUTHSCRBL=scribbles/author.scrbl
CWSCRBL = scribbles/copyright.scrbl

exe: compiled/rkt-ls compiled/rkt-echo compiled/rkt-stat compiled/rkt-head compiled/rkt-true compiled/rkt-false docs

compiled/rkt-ls: src/ls.rkt $(DUTIL)/human-size.rkt $(DUTIL)/human-date.rkt $(DUTIL)/fileaccessstr.rkt $(DLIBC)/stat.rkt $(DLIBC)/pwd.rkt $(DLIBC)/grp.rkt $(TYPEDEFS)/stat.rkt $(TYPEDEFS)/getpwuid.rkt $(TYPEDEFS)/getgrgid.rkt $(VERSIONFILE) 
	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

compiled/rkt-echo: src/echo.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-echo src/echo.rkt

compiled/rkt-stat: src/stat.rkt $(DLIBC)/stat.rkt $(DLIBC)/pwd.rkt $(DLIBC)/grp.rkt $(DUTIL)/fileaccessstr.rkt $(DUTIL)/fileaccessoct.rkt  $(TYPEDEFS)/stat.rkt $(TYPEDEFS)/getpwuid.rkt $(TYPEDEFS)/getgrgid.rkt $(REPL)/stat.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-stat src/stat.rkt

compiled/rkt-head: src/head.rkt $(REPL)/head.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-head src/head.rkt

compiled/rkt-true: src/true.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-true src/true.rkt

compiled/rkt-false: src/false.rkt $(VERSIONFILE)
	$(RACO) exe -o compiled/rkt-false src/false.rkt

docs: docs-html docs-md

docs-html: docs/html/ls.html docs/html/echo.html docs/html/stat.html docs/html/head.html docs/html/true.html docs/html/false.html

docs/html/ls.html: scribbles/ls.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/ls.scrbl

docs/html/echo.html: scribbles/echo.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/echo.scrbl

docs/html/stat.html: scribbles/stat.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/stat.scrbl

docs/html/head.html: scribbles/head.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/head.scrbl
	
docs/html/true.html: scribbles/true.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/true.scrbl
	
docs/html/false.html: scribbles/false.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribbles/false.scrbl

docs-md: docs/md/ls.md docs/md/echo.md docs/md/stat.md docs/md/head.md docs/md/true.md docs/md/false.md

docs/md/ls.md: scribbles/ls.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribbles/ls.scrbl

docs/md/echo.md: scribbles/echo.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribbles/echo.scrbl

docs/md/stat.md: scribbles/stat.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribbles/stat.scrbl

docs/md/head.md: scribbles/head.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribbles/head.scrbl

docs/md/true.md: scribbles/true.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribbles/true.scrbl

docs/md/false.md: scribbles/false.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribbles/false.scrbl

clean:
	rm -rf compiled
	rm -rf docs/

$(shell mkdir -p compiled)
