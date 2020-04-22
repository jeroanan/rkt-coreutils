RACO=raco
SCRIBBLE=scribble

DUTIL=src/util
DLIBC=src/libc
VERSIONFILE=src/util/version.rkt
TYPEDEFS=src/typedef
REPL=src/repl

AUTHSCRBL=scribblings/author.scrbl
CWSCRBL = scribblings/copyright.scrbl

exe: compiled/rkt-ls compiled/rkt-echo compiled/rkt-stat compiled/rkt-head compiled/rkt-true compiled/rkt-false compiled/rkt-whoami compiled/rkt-cat compiled/rkt-md5sum docs

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

compiled/rkt-whoami: src/whoami.rkt $(VERSIONFILE) $(REPL)/whoami.rkt $(DLIBC)/pwd.rkt $(DLIBC)/unistd.rkt $(TYPEDEFS)/getpwuid.rkt
	$(RACO) exe -o compiled/rkt-whoami src/whoami.rkt

compiled/rkt-cat: src/cat.rkt $(VERSIONFILE) $(REPL)/cat.rkt $(DUTIL)/programs.rkt
	$(RACO) exe -o compiled/rkt-cat src/cat.rkt

compiled/rkt-md5sum: src/md5sum.rkt $(VERSIONFILE) $(REPL)/md5sum.rkt $(DUTIL)/programs.rkt
	$(RACO) exe -o compiled/rkt-md5sum src/md5sum.rkt
	
docs: docs-html docs-md

docs-html: docs/html/ls.html docs/html/echo.html docs/html/stat.html docs/html/head.html docs/html/true.html docs/html/false.html docs/html/whoami.html docs/html/cat.html docs/html/md5sum.html

docs/html/ls.html: scribblings/ls.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/ls.scrbl

docs/html/echo.html: scribblings/echo.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/echo.scrbl

docs/html/stat.html: scribblings/stat.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/stat.scrbl

docs/html/head.html: scribblings/head.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/head.scrbl
	
docs/html/true.html: scribblings/true.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/true.scrbl
	
docs/html/false.html: scribblings/false.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/false.scrbl

docs/html/whoami.html: scribblings/whoami.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/whoami.scrbl

docs/html/cat.html: scribblings/cat.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/cat.scrbl

docs/html/md5sum.html: scribblings/md5sum.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/html --html scribblings/md5sum.scrbl

docs-md: docs/md/ls.md docs/md/echo.md docs/md/stat.md docs/md/head.md docs/md/true.md docs/md/false.md docs/md/whoami.md docs/md/cat.md docs/md/md5sum.md

docs/md/ls.md: scribblings/ls.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/ls.scrbl

docs/md/echo.md: scribblings/echo.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/echo.scrbl

docs/md/stat.md: scribblings/stat.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/stat.scrbl

docs/md/head.md: scribblings/head.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/head.scrbl

docs/md/true.md: scribblings/true.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/true.scrbl

docs/md/false.md: scribblings/false.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/false.scrbl

docs/md/whoami.md: scribblings/whoami.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/whoami.scrbl

docs/md/cat.md: scribblings/cat.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/cat.scrbl

docs/md/md5sum.md: scribblings/md5sum.scrbl $(AUTHSCRBL) $(CWSCRBL)
	$(SCRIBBLE) --dest docs/md --markdown scribblings/md5sum.scrbl

clean:
	rm -rf compiled
	rm -rf docs/

$(shell mkdir -p compiled)
