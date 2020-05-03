RACO=raco
SCRIBBLE=scribble

DEPDIR=~/bin

AUTHSCRBL=scribblings/author.scrbl
CWSCRBL = scribblings/copyright.scrbl

HDOCS=docs/html
MDDOCS=docs/md
SCRIBDIR=scribblings

DOCDEPS=$(AUTHSCRBL) $(CWSCRBL)

all: make-all docs

make-all: 
	$(RACO) make src/*.rkt

docs: docs-html docs-md

docs-html: \
	$(HDOCS)/base64.html \
	$(HDOCS)/cat.html \
	$(HDOCS)/echo.html \
	$(HDOCS)/false.html \
	$(HDOCS)/groups.html \
	$(HDOCS)/head.html \
	$(HDOCS)/id.html \
	$(HDOCS)/ls.html \
	$(HDOCS)/md5sum.html \
	$(HDOCS)/nl.html \
	$(HDOCS)/sha1sum.html \
	$(HDOCS)/sha224sum.html \
	$(HDOCS)/sha256sum.html \
	$(HDOCS)/sha384sum.html \
	$(HDOCS)/sha512sum.html \
	$(HDOCS)/sort.html \
	$(HDOCS)/stat.html \
	$(HDOCS)/tac.html \
	$(HDOCS)/tail.html \
	$(HDOCS)/true.html \
	$(HDOCS)/users.html \
	$(HDOCS)/who.html \
	$(HDOCS)/whoami.html \

$(HDOCS)/base64.html: \
	$(SCRIBDIR)/base64.scrbl \
	$(DOCDEPS)

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/base64.scrbl

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

$(HDOCS)/groups.html: \
	$(SCRIBDIR)/groups.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/groups.scrbl

$(HDOCS)/head.html: \
	$(SCRIBDIR)/head.scrbl \
	$(DOCDEPS) 
	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/head.scrbl

$(HDOCS)/id.html: \
	$(SCRIBDIR)/id.scrbl \
	$(DOCDEPS) 
	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/id.scrbl

$(HDOCS)/ls.html: \
	$(SCRIBDIR)/ls.scrbl \
	$(DOCDEPS)

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/ls.scrbl

$(HDOCS)/md5sum.html: \
	$(SCRIBDIR)/md5sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/md5sum.scrbl

$(HDOCS)/nl.html: \
	$(SCRIBDIR)/nl.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/nl.scrbl

$(HDOCS)/sha1sum.html: \
	$(SCRIBDIR)/sha1sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/sha1sum.scrbl
	
$(HDOCS)/sha224sum.html: \
	$(SCRIBDIR)/sha224sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/sha224sum.scrbl
	
$(HDOCS)/sha256sum.html: \
	$(SCRIBDIR)/sha256sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/sha256sum.scrbl
	
$(HDOCS)/sha384sum.html: \
	$(SCRIBDIR)/sha384sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/sha384sum.scrbl
	
$(HDOCS)/sha512sum.html: \
	$(SCRIBDIR)/sha512sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/sha512sum.scrbl
	
$(HDOCS)/sort.html: \
	$(SCRIBDIR)/sort.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/sort.scrbl
	
$(HDOCS)/stat.html: \
	$(SCRIBDIR)/stat.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/stat.scrbl
	
$(HDOCS)/tac.html: \
	$(SCRIBDIR)/tac.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/tac.scrbl
	
$(HDOCS)/tail.html: \
	$(SCRIBDIR)/tail.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/tail.scrbl
	
$(HDOCS)/true.html: \
	$(SCRIBDIR)/true.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/true.scrbl

$(HDOCS)/users.html: \
	$(SCRIBDIR)/users.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/users.scrbl

$(HDOCS)/whoami.html: \
	$(SCRIBDIR)/whoami.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/whoami.scrbl

$(HDOCS)/who.html: \
	$(SCRIBDIR)/who.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(HDOCS) --html $(SCRIBDIR)/who.scrbl

docs-md: \
	README.md \
	BUILDING.md \
	$(MDDOCS)/base64.md \
	$(MDDOCS)/cat.md \
	$(MDDOCS)/echo.md \
	$(MDDOCS)/false.md \
	$(MDDOCS)/head.md \
	$(MDDOCS)/id.md \
	$(MDDOCS)/groups.md \
	$(MDDOCS)/ls.md \
	$(MDDOCS)/md5sum.md \
	$(MDDOCS)/nl.md \
	$(MDDOCS)/sha1sum.md \
	$(MDDOCS)/sha224sum.md \
	$(MDDOCS)/sha256sum.md \
	$(MDDOCS)/sha384sum.md \
	$(MDDOCS)/sha512sum.md \
	$(MDDOCS)/sort.md \
	$(MDDOCS)/stat.md \
	$(MDDOCS)/tac.md \
	$(MDDOCS)/tail.md \
	$(MDDOCS)/true.md \
	$(MDDOCS)/users.md \
	$(MDDOCS)/whoami.md \
	$(MDDOCS)/whoami.md

README.md: $(SCRIBDIR)/README.scrbl
	$(SCRIBBLE) --dest . --markdown $(SCRIBDIR)/README.scrbl

BUILDING.md: $(SCRIBDIR)/BUILDING.scrbl
	$(SCRIBBLE) --dest . --markdown $(SCRIBDIR)/BUILDING.scrbl

$(MDDOCS)/base64.md: \
	$(SCRIBDIR)/base64.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/base64.scrbl

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

$(MDDOCS)/groups.md: \
	$(SCRIBDIR)/groups.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/groups.scrbl

$(MDDOCS)/head.md: \
	$(SCRIBDIR)/head.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/head.scrbl

$(MDDOCS)/id.md: \
	$(SCRIBDIR)/id.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/id.scrbl

$(MDDOCS)/ls.md: \
	$(SCRIBDIR)/ls.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/ls.scrbl

$(MDDOCS)/md5sum.md: \
	$(SCRIBDIR)/md5sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/md5sum.scrbl

$(MDDOCS)/nl.md: \
	$(SCRIBDIR)/nl.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/nl.scrbl

$(MDDOCS)/sha1sum.md: \
	$(SCRIBDIR)/sha1sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/sha1sum.scrbl

$(MDDOCS)/sha224sum.md: \
	$(SCRIBDIR)/sha224sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/sha224sum.scrbl

$(MDDOCS)/sha256sum.md: \
	$(SCRIBDIR)/sha256sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/sha256sum.scrbl

$(MDDOCS)/sha384sum.md: \
	$(SCRIBDIR)/sha384sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/sha384sum.scrbl

$(MDDOCS)/sha512sum.md: \
	$(SCRIBDIR)/sha512sum.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/sha512sum.scrbl

$(MDDOCS)/sort.md: \
	$(SCRIBDIR)/sort.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/sort.scrbl

$(MDDOCS)/stat.md: \
	$(SCRIBDIR)/stat.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/stat.scrbl

$(MDDOCS)/tac.md: \
	$(SCRIBDIR)/tac.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/tac.scrbl

$(MDDOCS)/tail.md: \
	$(SCRIBDIR)/tail.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/tail.scrbl

$(MDDOCS)/true.md: \
	$(SCRIBDIR)/true.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/true.scrbl

$(MDDOCS)/users.md: \
	$(SCRIBDIR)/users.scrbl \
	$(DOCDEPS) 

	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/users.scrbl

$(MDDOCS)/whoami.md: \
	$(SCRIBDIR)/whoami.scrbl \
	$(DOCDEPS) 
	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/whoami.scrbl

$(MDDOCS)/who.md: \
	$(SCRIBDIR)/who.scrbl \
	$(DOCDEPS) 
	$(SCRIBBLE) --dest $(MDDOCS) --markdown $(SCRIBDIR)/who.scrbl

launchers:
	./make-launchers.sh

clean:
	rm -rf docs/

deploy:
	cp -f compiled/* $(DEPDIR)

$(shell mkdir -p compiled)
