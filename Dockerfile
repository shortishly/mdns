FROM shortishly/erlang
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>
ADD . /code
ENV PATH /code:$PATH
WORKDIR /code
ENTRYPOINT erl -pa ebin -pa deps/*/ebin -boot start_sasl -s mdns -noinput -name mdns@$HOSTNAME
