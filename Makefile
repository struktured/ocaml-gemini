
build:
	jbuilder build

install: build
	jbuilder install

default:
	build

clean :
	jbuilder clean

test: build
	jbuilder build @runtest
