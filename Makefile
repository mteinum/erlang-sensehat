sensehat_drv : sensehat_drv.c
	mkdir -p ebin
	cc -o ebin/sensehat_drv.so -fpic -shared -Wall -Wextra -Wformat sensehat_drv.c

sensestick_drv: sensestick_drv.c
	mkdir -p ebin
	cc -o ebin/sensestick_drv.so -fpic -shared -Wall -Wextra -Wformat sensestick_drv.c	

erl:
	mkdir -p ebin
	erl -make

all: erl sensehat_drv sensestick_drv

clean:
	rm -f ebin/*.so ebin/*.beam
