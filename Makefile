.PHONY: slack

default: slack

SYSTEM := $(shell uname -s)

ifeq ($(SYSTEM),Darwin)
SSL-BASE :=$(lastword $(wildcard /usr/local/Cellar/openssl/*/))
SED := sed
MYSQL-BASE := $(lastword $(wildcard /usr/local/Cellar/mysql/*/))
LIBYAML-BASE := $(lastword $(wildcard /usr/local/Cellar/libyaml/*/))
#$(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
#$(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
else
LDFLAGS := "-L/usr/lib -lssl -lyaml"
CPPFLAGS := "-I/usr/include"
LIBYAML-BASE := "/usr/include"
SED := sed
endif

slack: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
slack: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
slack:
	gxc -O -o sla -static -exe -g -genv -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc -gsc-flag -keep-c slack/sla.ss

linux-static:
	docker run -e GERBIL_PATH=/dd/.gerbil -v $(PWD):/dd -it jaimef/centos bash -c 'cd /dd && make linux-static-intern'

linux-static-intern:
	cd /dd && gxpkg link ober/slack . || true
	gxpkg uninstall github.com/ober/oberlib
	gxpkg install github.com/ober/oberlib
	gxpkg build ober/slack
	gxc -o slack-static -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -static -ld-options "-static -lpthread -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -gsc-option -prelude '(declare (not safe))' -exe slack/slack.ss
