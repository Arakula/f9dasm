PROGS=f9dasm hex2bin mot2bin

CCFLAGS=-O2 -Wall -Wextra

all: $(PROGS)

%: %.c
	$(CC) $(CCFLAGS) $(<) -o $@

.PHONY clean:
	rm $(PROGS)
