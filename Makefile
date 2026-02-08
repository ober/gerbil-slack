PROJECT := slack
ARCH := $(shell uname -m)
PWD := $(shell pwd)
GERBIL_HOME := /opt/gerbil
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
UID := $(shell id -u)
GID := $(shell id -g)
GERBIL_LOADPATH_EXTRA := $(HOME)/.gerbil/lib

default: build

check-root:
	@if [ "${UID}" -eq 0 ]; then \
	git config --global --add safe.directory /src; \
	fi

build: check-root
	GERBIL_LOADPATH="$(GERBIL_LOADPATH_EXTRA)" gerbil build

test:
	gerbil test ./...

clean:
	rm -rf .gerbil

linux-static-docker: clean
	docker run -t \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src build

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)

.PHONY: default build test clean linux-static-docker install check-root
