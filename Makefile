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

#$(info $(CPPFLAGS) is cppflags)
#$(info $(LDFLAGS) is ldflags)

slack: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
slack: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
slack:
	gxc -O -o dda -static -exe -g -genv -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc -gsc-flag -keep-c slack/dda.ss

linux-static:
	docker run -e GERBIL_PATH=/dd/.gerbil -e PATH='/root/gerbil/bin:/usr/local/gambit/current/bin:/bin:/usr/bin:/sbin:/usr/sbin' -v $(PWD):/dd -it jaimef/centos bash -c 'cd /dd && make linux-static-intern'

linux-static-intern:
	gxc -o dda-linux-static -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -gsc-option -prelude '(declare (not safe))' -exe slack/dda.ss

linux:
	gxc -o dd-linux -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -exe slack/dda.ss

linux-debug:
	gxc -o dd-linux -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -g -gsrc -genv -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -exe slack/dda.ss

fast: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
fast: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
fast:
	gxc -O -o slack -exe -g -genv -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc -gsc-flag -keep-c slack.ss

docker:
	docker build --rm=true -t slack .
	docker tag slack jaimef/slack
	docker push jaimef/slack
