RACO=raco
SCRIBBLE=scribble

DUTIL=src/util
DLIBC=src/libc
VERSIONFILE=src/util/version.rkt
TYPEDEFS=src/typedef
REPL=src/repl

AUTHSCRBL=scribblings/author.scrbl
CWSCRBL = scribblings/copyright.scrbl

HDOCS=docs/html
MDDOCS=docs/md
SCRIBDIR=scribblings

DOCDEPS=$(AUTHSCRBL) $(CWSCRBL)

all: exe docs

exe: \
	compiled/rkt-cat \
	compiled/rkt-echo \
	compiled/rkt-false \
	compiled/rkt-head \
	compiled/rkt-ls \
	compiled/rkt-md5sum \
	compiled/rkt-stat \
	compiled/rkt-true \
	compiled/rkt-whoami \

compiled/rkt-cat: \
	src/cat.rkt \
	$(VERSIONFILE) \
	$(REPL)/cat.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(DUTIL)/version.rkt
	
	$(RACO) exe -o compiled/rkt-cat src/cat.rkt

compiled/rkt-echo: \
	src/echo.rkt \
	$(VERSIONFILE)

	$(RACO) exe -o compiled/rkt-echo src/echo.rkt

compiled/rkt-false: \
	src/false.rkt \
	$(DUTIL)/truefalseprogram.rkt \
	$(VERSIONFILE)

	$(RACO) exe -o compiled/rkt-false src/false.rkt

compiled/rkt-head: \
	src/head.rkt \
	$(REPL)/head.rkt \
	$(DUTIL)/member.rkt \
	$(REPL)/util/util.rkt \
	$(REPL)/util/line-by-line-processor.rkt \
	$(VERSIONFILE)

	$(RACO) exe -o compiled/rkt-head src/head.rkt

compiled/rkt-ls: \
	src/ls.rkt \
	$(DUTIL)/human-size.rkt \
	$(DUTIL)/human-date.rkt \
	$(DUTIL)/fileaccessstr.rkt \
	$(DUTIL)/member.rkt \
	$(DLIBC)/stat.rkt \
	$(DLIBC)/pwd.rkt \
	$(DLIBC)/grp.rkt \
	$(TYPEDEFS)/stat.rkt \
	$(TYPEDEFS)/getpwuid.rkt \
	$(TYPEDEFS)/getgrgid.rkt \
	$(VERSIONFILE) 

	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

compiled/rkt-md5sum: \
	src/md5sum.rkt \
	$(VERSIONFILE) \
	$(REPL)/md5sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt

	$(RACO) exe -o compiled/rkt-md5sum src/md5sum.rkt

compiled/rkt-stat: \
	src/stat.rkt \
	$(DLIBC)/stat.rkt \
	$(DLIBC)/pwd.rkt \
	$(DLIBC)/grp.rkt \
	$(DUTIL)/fileaccessstr.rkt \
	$(DUTIL)/fileaccessoct.rkt  \
	$(TYPEDEFS)/stat.rkt \
	$(TYPEDEFS)/getpwuid.rkt \
	$(TYPEDEFS)/getgrgid.rkt \
	$(REPL)/stat.rkt \
	$(VERSIONFILE)

	$(RACO) exe -o compiled/rkt-stat src/stat.rkt

compiled/rkt-true: \
	src/true.rkt \
	$(DUTIL)/truefalseprogram.rkt \
	$(VERSIONFILE)

	$(RACO) exe -o compiled/rkt-true src/true.rkt

compiled/rkt-whoami: \
	src/whoami.rkt \
	$(VERSIONFILE) \
	$(REPL)/whoami.rkt \
	$(DLIBC)/pwd.rkt \
	$(DLIBC)/unistd.rkt \
	$(TYPEDEFS)/getpwuid.rkt

	$(RACO) exe -o compiled/rkt-whoami src/whoami.rkt
	
docs: docs-html docs-md

docs-html: \
	$(HDOCS)/cat.html \
	$(HDOCS)/echo.html \
	$(HDOCS)/false.html \
	$(HDOCS)/head.html \
	$(HDOCS)/ls.html \
	$(HDOCS)/md5sum.html \
	$(HDOCS)/stat.html \
	$(HDOCS)/true.html \
	$(HDOCS)/whoami.html \

$(HDOCS)/cat.html: \
	$(SCRIBDIR)/cat.scrbl \
	$(DOCDEPS)

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/cat.scrbl

$(HDOCS)/echo.html: \
	$(SCRIBDIR)/echo.scrbl \
	$(DOCDEPS)

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/echo.scrbl

$(HDOCS)/false.html: \
	$(SCRIBDIR)/false.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/false.scrbl

$(HDOCS)/head.html: \
	$(SCRIBDIR)/head.scrbl \
	$(DOCDEPS) 
	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/head.scrbl

$(HDOCS)/ls.html: \
	$(SCRIBDIR)/ls.scrbl \
	$(DOCDEPS)

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/ls.scrbl


$(HDOCS)/md5sum.html: \
	$(SCRIBDIR)/md5sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/md5sum.scrbl

$(HDOCS)/stat.html: \
	$(SCRIBDIR)/stat.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/stat.scrbl
	
$(HDOCS)/true.html: \
	$(SCRIBDIR)/true.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/true.scrbl

$(HDOCS)/whoami.html: \
	$(SCRIBDIR)/whoami.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/whoami.scrbl

docs-md: \
	$(MDDOCS)/cat.md \
	$(MDDOCS)/echo.md \
	$(MDDOCS)/false.md \
	$(MDDOCS)/head.md \
	$(MDDOCS)/ls.md \
	$(MDDOCS)/md5sum.md \
	$(MDDOCS)/stat.md \
	$(MDDOCS)/true.md \
	$(MDDOCS)/whoami.md 

$(MDDOCS)/cat.md: \
	$(SCRIBDIR)/cat.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/cat.scrbl

$(MDDOCS)/echo.md: \
	$(SCRIBDIR)/echo.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/echo.scrbl

$(MDDOCS)/false.md: \
	$(SCRIBDIR)/false.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/false.scrbl

$(MDDOCS)/head.md: \
	$(SCRIBDIR)/head.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/head.scrbl

$(MDDOCS)/ls.md: \
	$(SCRIBDIR)/ls.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/ls.scrbl

$(MDDOCS)/md5sum.md: \
	$(SCRIBDIR)/md5sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/md5sum.scrbl

$(MDDOCS)/stat.md: \
	$(SCRIBDIR)/stat.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/stat.scrbl

$(MDDOCS)/true.md: \
	$(SCRIBDIR)/true.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/true.scrbl

$(MDDOCS)/whoami.md: \
	$(SCRIBDIR)/whoami.scrbl \
	$(DOCDEPS) 
	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/whoami.scrbl

clean:
	rm -rf compiled
	rm -rf docs/

$(shell mkdir -p compiled)
