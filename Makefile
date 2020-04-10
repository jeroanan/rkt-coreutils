RACO=raco

all: compiled/rkt-ls

compiled/rkt-ls: src/ls.rkt src/human-size.rkt src/human-date.rkt src/libc/stat.rkt src/libc/pwd.rkt src/libc/grp.rkt 
	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

clean:
	rm -rf compiled

$(shell mkdir -p compiled)
