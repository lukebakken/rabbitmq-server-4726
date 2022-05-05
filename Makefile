.PHONY: clean run_tls_server run_tls_client

all: tls_server tls_client

clean:
	rm -f *.beam

tls_server.beam: tls_server.erl
	erlc +debug tls_server.erl

tls_server: tls_server.beam

tls_client.beam: tls_client.erl
	erlc +debug tls_client.erl

tls_client: tls_client.beam

run_tls_client: tls_client
	erl -noinput -s tls_client start

run_tls_server: tls_server
	erl -noinput -s tls_server start
