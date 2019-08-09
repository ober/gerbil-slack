.PHONY: slack
default: slack

SYSTEM := $(shell uname -s)

ifeq ($(SYSTEM),Darwin)
SSL-BASE :=$(lastword $(wildcard /usr/local/Cellar/openssl/*/))
SED := sed
LIBYAML-BASE := $(lastword $(wildcard /usr/local/Cellar/libyaml/*/))
$(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml")
$(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include")
else
LDFLAGS := "-L/usr/lib -lssl -lyaml"
CPPFLAGS := "-I/usr/include"
LIBYAML-BASE := "/usr/include"
SED := sed
endif

slack: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib  -L/usr/local/lib -lz -lssl -lyaml")
slack: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
slack:
	./build.ss static

docker:
	docker build --rm=true -t slack .
	docker tag slack jaimef/slack

push:
	docker push jaimef/slack
