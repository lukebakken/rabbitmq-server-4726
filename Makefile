.PHONY: clean run_openssl_client run_openssl_server

PORT ?= 9999
SERVER ?= localhost

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
	erl -noinput -s tls_client start $(PORT)

run_tls_server: tls_server
	erl -noinput -s tls_server start $(PORT)

run_openssl_client:
	openssl s_client -connect $(SERVER):$(PORT) \
		-CAfile $(CURDIR)/certs/ca_certificate.pem \
		-cert $(CURDIR)/certs/client_certificate.pem \
		-key $(CURDIR)/certs/client_key.pem \
		-verify 8 -verify_hostname $(SERVER)

run_openssl_server:
	openssl s_server -accept $(PORT) \
		-CAfile $(CURDIR)/certs/ca_certificate.pem \
		-cert $(CURDIR)/certs/server_localhost_certificate.pem \
		-key $(CURDIR)/certs/server_localhost_key.pem
