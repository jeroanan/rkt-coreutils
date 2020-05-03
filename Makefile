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
	$(HDOCS)/stat.html \
	$(HDOCS)/true.html \
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
	$(MDDOCS)/stat.md \
	$(MDDOCS)/true.md \
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

launchers:
	./make-launchers.sh

clean:
	rm -rf docs/

deploy:
	cp -f compiled/* $(DEPDIR)

$(shell mkdir -p compiled)
