MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
CC = g++
CFLAGS = -O2 -Wall -std=c++17 -Wno-unknown-pragmas

.PHONY: solver clean


%.o: %.cpp
	$(CC) $(CFLAGS) -o $@ -c $<

solver: json/jsoncpp.o common.o main.o
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -rf *.o
