RACO=raco
SCRIBBLE=scribble

DEPDIR=~/bin

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
	compiled/rkt-base64 \
	compiled/rkt-cat \
	compiled/rkt-echo \
	compiled/rkt-false \
	compiled/rkt-head \
	compiled/rkt-ls \
	compiled/rkt-md5sum \
	compiled/rkt-nl \
	compiled/rkt-stat \
	compiled/rkt-sha1sum \
	compiled/rkt-sha224sum \
	compiled/rkt-sha256sum \
	compiled/rkt-sha384sum \
	compiled/rkt-sha512sum \
	compiled/rkt-sort \
	compiled/rkt-tac \
	compiled/rkt-tail \
	compiled/rkt-true \
	compiled/rkt-uniq \
	compiled/rkt-whoami

compiled/rkt-base64: demods/rkt-base64.zo
	$(RACO) exe -o compiled/rkt-base64 demods/rkt-base64.zo

demods/rkt-base64.zo: \
	src/base64.rkt \
	$(VERSIONFILE) \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(REPL)/base64.rkt \
	$(REPL)/util/util.rkt \
	$(REPL)/util/file-by-file-processor.rkt

	$(RACO) demod -o demods/rkt-base64.zo src/base64.rkt

compiled/rkt-cat: demods/rkt-cat.zo
	$(RACO) exe -o compiled/rkt-cat demods/rkt-cat.zo

demods/rkt-cat.zo: \
	src/cat.rkt \
	$(VERSIONFILE) \
	$(REPL)/cat.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(DUTIL)/version.rkt
	
	$(RACO) demod -o demods/rkt-cat.zo src/cat.rkt

compiled/rkt-echo: demods/rkt-echo.zo
	$(RACO) exe -o compiled/rkt-echo demods/rkt-echo.zo

demods/rkt-echo.zo: \
	src/echo.rkt \
	$(VERSIONFILE)

	$(RACO) demod -o demods/rkt-echo.zo src/echo.rkt

compiled/rkt-false: demods/rkt-false.zo 
	$(RACO) exe -o compiled/rkt-false demods/rkt-false.zo

demods/rkt-false.zo: \
	src/false.rkt \
	$(DUTIL)/truefalseprogram.rkt \
	$(VERSIONFILE)

	$(RACO) demod -o demods/rkt-false.zo src/false.rkt

compiled/rkt-uniq: demods/rkt-uniq.zo
	$(RACO) exe -o compiled/rkt-uniq demods/rkt-uniq.zo

demods/rkt-uniq.zo: \
	src/uniq.rkt \
	$(REPL)/uniq.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(DUTIL)/member.rkt \
	$(DUTIL)/stringutil.rkt \
	$(REPL)/util/util.rkt \
	$(REPL)/util/line-by-line-processor.rkt \
	$(VERSIONFILE)

	$(RACO) demod -o demods/rkt-uniq.zo src/uniq.rkt

compiled/rkt-head: demods/rkt-head.zo 
	$(RACO) exe -o compiled/rkt-head demods/rkt-head.zo

demods/rkt-head.zo: \
	src/head.rkt \
	$(REPL)/head.rkt \
	$(DUTIL)/member.rkt \
	$(REPL)/util/util.rkt \
	$(REPL)/util/line-by-line-processor.rkt \
	$(VERSIONFILE)

	$(RACO) demod -o demods/rkt-head.zo src/head.rkt

compiled/rkt-ls: demods/rkt-ls.zo
	$(RACO) exe -o compiled/rkt-ls demods/rkt-ls.zo

demods/rkt-ls.zo: \
	src/ls.rkt \
	$(DUTIL)/human-size.rkt \
	$(DUTIL)/human-date.rkt \
	$(DUTIL)/fileaccessstr.rkt \
	$(DUTIL)/member.rkt \
	$(DUTIL)/stringutil.rkt \
	$(DLIBC)/stat.rkt \
	$(DLIBC)/pwd.rkt \
	$(DLIBC)/grp.rkt \
	$(TYPEDEFS)/stat.rkt \
	$(TYPEDEFS)/getpwuid.rkt \
	$(TYPEDEFS)/getgrgid.rkt \
	$(VERSIONFILE) 

	$(RACO) demod -o demods/rkt-ls.zo src/ls.rkt

compiled/rkt-md5sum: demods/rkt-md5sum.zo
	$(RACO) exe -o compiled/rkt-md5sum demods/rkt-md5sum.zo

demods/rkt-md5sum.zo: \
	src/md5sum.rkt \
	$(VERSIONFILE) \
	$(REPL)/md5sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt

	$(RACO) demod -o demods/rkt-md5sum.zo src/md5sum.rkt

compiled/rkt-nl: demods/rkt-nl.zo
	$(RACO) exe -o compiled/rkt-nl demods/rkt-nl.zo

demods/rkt-nl.zo: \
	src/nl.rkt \
	$(REPL)/nl.rkt \
	$(VERSIONFILE) \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(REPL)/util/program/line-by-line-processor-program.rkt

	$(RACO) demod -o demods/rkt-nl.zo src/nl.rkt

compiled/rkt-stat: demods/rkt-stat.zo
	$(RACO) exe -o compiled/rkt-stat demods/rkt-stat.zo

demods/rkt-stat.zo: \
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

	$(RACO) demod -o demods/rkt-stat.zo src/stat.rkt

compiled/rkt-sha1sum: demods/rkt-sha1sum.zo
	$(RACO) exe -o compiled/rkt-sha1sum demods/rkt-sha1sum.zo

demods/rkt-sha1sum.zo: \
	src/sha1sum.rkt \
	$(REPL)/sha1sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/program/shaprogram.rkt

	$(RACO) demod -o demods/rkt-sha1sum.zo src/sha1sum.rkt

compiled/rkt-sha224sum: demods/rkt-sha224sum.zo
	$(RACO) exe -o compiled/rkt-sha224sum demods/rkt-sha224sum.zo

demods/rkt-sha224sum.zo: \
	src/sha224sum.rkt \
	$(REPL)/sha224sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/program/shaprogram.rkt

	$(RACO) demod -o demods/rkt-sha224sum.zo src/sha224sum.rkt

compiled/rkt-sha256sum: demods/rkt-sha256sum.zo
	$(RACO) exe -o compiled/rkt-sha256sum demods/rkt-sha256sum.zo

demods/rkt-sha256sum.zo: \
	src/sha256sum.rkt \
	$(REPL)/sha256sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/program/shaprogram.rkt

	$(RACO) demod -o demods/rkt-sha256sum.zo src/sha256sum.rkt

compiled/rkt-sha384sum: demods/rkt-sha384sum.zo
	$(RACO) exe -o compiled/rkt-sha384sum demods/rkt-sha384sum.zo

demods/rkt-sha384sum.zo: \
	src/sha384sum.rkt \
	$(REPL)/sha384sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/program/shaprogram.rkt

	$(RACO) demod -o demods/rkt-sha384sum.zo src/sha384sum.rkt

compiled/rkt-sha512sum: demods/rkt-sha512sum.zo
	$(RACO) exe -o compiled/rkt-sha512sum demods/rkt-sha512sum.zo

demods/rkt-sha512sum.zo: \
	src/sha512sum.rkt \
	$(REPL)/sha512sum.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/program/shaprogram.rkt

	$(RACO) demod -o demods/rkt-sha512sum.zo src/sha512sum.rkt

compiled/rkt-tac: demods/rkt-tac.zo
	$(RACO) exe -o compiled/rkt-tac demods/rkt-tac.zo

demods/rkt-tac.zo: \
	src/tac.rkt \
	$(REPL)/tac.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/util.rkt \
	$(REPL)/util/file-by-file-processor.rkt

	$(RACO) demod -o demods/rkt-tac.zo src/tac.rkt

compiled/rkt-sort: demods/rkt-sort.zo
	$(RACO) exe -o compiled/rkt-sort demods/rkt-sort.zo

demods/rkt-sort.zo: \
	src/sort.rkt \
	$(REPL)/sort.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(DUTIL)/member.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/util.rkt \
	$(REPL)/util/file-by-file-processor.rkt

	$(RACO) demod -o demods/rkt-sort.zo src/sort.rkt

compiled/rkt-tail: demods/rkt-tail.zo
	$(RACO) exe -o compiled/rkt-tail demods/rkt-tail.zo

demods/rkt-tail.zo: \
	src/tail.rkt \
	$(REPL)/tail.rkt \
	$(DUTIL)/simple-file-handler-program.rkt \
	$(VERSIONFILE) \
	$(REPL)/util/util.rkt \
	$(REPL)/util/file-by-file-processor.rkt

	$(RACO) demod -o demods/rkt-tail.zo src/tail.rkt

compiled/rkt-true: demods/rkt-true.zo
	$(RACO) exe -o compiled/rkt-true demods/rkt-true.zo

demods/rkt-true.zo: \
	src/true.rkt \
	$(DUTIL)/truefalseprogram.rkt \
	$(VERSIONFILE)

	$(RACO) demod -o demods/rkt-true.zo src/true.rkt

compiled/rkt-whoami: demods/rkt-whoami.zo
	$(RACO) exe -o compiled/rkt-whoami demods/rkt-whoami.zo

demods/rkt-whoami.zo: \
	src/whoami.rkt \
	$(VERSIONFILE) \
	$(DUTIL)/simple-program.rkt \
	$(REPL)/whoami.rkt \
	$(DLIBC)/pwd.rkt \
	$(DLIBC)/unistd.rkt \
	$(TYPEDEFS)/getpwuid.rkt

	$(RACO) demod -o demods/rkt-whoami.zo src/whoami.rkt
	
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

deploy:
	cp -f compiled/* $(DEPDIR)

$(shell mkdir -p compiled)
$(shell mkdir -p demods)
