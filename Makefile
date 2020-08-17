mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
TCPATH := $(dir ${mkfile_path})

all:
	export GOPATH=${TCPATH}; go install t9;

clean:
	rm -rf bin
