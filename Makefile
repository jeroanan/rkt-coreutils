RACO=raco

all: compiled/rkt-ls

compiled/rkt-ls: src/ls.rkt src/libc/stat.rkt 
	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

clean:
	rm -rf compiled

$(shell mkdir -p compiled)
