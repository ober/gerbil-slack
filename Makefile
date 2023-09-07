PROJECT := slack

NAME := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
DOCKER_IMAGE := "gerbil/alpine:latest"

$(info "name is " $(NAME))
$(eval uid := $(shell id -u))
$(eval gid := $(shell id -g))

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build $(PROJECT)

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/tmp/.gerbil \
	-u "$(uid):$(gid)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src linux-static

linux-static: build
	/opt/gerbil/bin/gxc -o $(PROJECT)-bin -static \
	-cc-options "-Bstatic" \
	-ld-options "-static -lpthread -lyaml -lz" \
	-exe $(PROJECT)/$(PROJECT).ss

clean:
	rm -f $(PROJECT)-bin

install:
	mv $(PROJECT)-bin /usr/local/bin/$(PROJECT)
