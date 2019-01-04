from jaimef/gerbil

MAINTAINER jaimef@linbsd.org
COPY . /root/slack
ENV PATH "$PATH:/root/gerbil/bin"
ENV GERBIL_HOME "/root/gerbil"
RUN cd /root/slack && ./build.ss static
RUN cp /root/slack/slack /bin/slack

CMD /bin/bash
