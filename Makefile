ERL_INCLUDE ?= $(shell erl -eval 'io:format("~ts", [lists:concat([code:root_dir(), "/usr/include"])])' -s init stop -noshell)

sensehat_drv : c_src/sensehat_drv.c
	mkdir -p ebin
	cc -o ebin/sensehat_drv.so -fpic -shared -Wall -Wextra -Wformat -I"$(ERL_INCLUDE)" c_src/sensehat_drv.c

sensestick_drv: c_src/sensestick_drv.c
	mkdir -p ebin
	cc -o ebin/sensestick_drv.so -fpic -shared -Wall -Wextra -Wformat -I"$(ERL_INCLUDE)" c_src/sensestick_drv.c	

erl:
	mkdir -p ebin
	erl -make

all: erl sensehat_drv sensestick_drv

clean:
	rm -f ebin/*.so ebin/*.beam
