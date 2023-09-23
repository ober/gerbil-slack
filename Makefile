PROJECT := slack

NAME := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)"

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/mighty-gerbils/gerbil-libyaml
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build $(PROJECT)

linux-static-docker: clean
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-e USER=$(USER) \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	/bin/sh
#	make -C /src build

clean:
	rm -rf .gerbil

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
