RACO=raco

all: compiled/rkt-ls compiled/rkt-echo

compiled/rkt-ls: src/ls.rkt src/util/human-size.rkt src/util/human-date.rkt src/libc/stat.rkt src/libc/pwd.rkt src/libc/grp.rkt 
	$(RACO) exe -o compiled/rkt-ls src/ls.rkt

compiled/rkt-echo: src/echo.rkt
	$(RACO) exe -o compiled/rkt-echo src/echo.rkt

clean:
	rm -rf compiled

$(shell mkdir -p compiled)
