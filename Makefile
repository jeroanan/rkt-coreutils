RACO=raco

all: compiled/rkt-ls

compiled/rkt-ls: ls.rkt stat.rkt 
	$(RACO) exe -o compiled/rkt-ls ls.rkt

clean:
	rm -rf compiled

$(shell mkdir -p compiled)
