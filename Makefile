.PHONY: clean run_openssl_client run_openssl_server

PORT ?= 9999
HOST ?= localhost
DN ?= CN=undefined
PASSWD ?= test1234

all: eldap_client tls_server tls_client

clean:
	rm -f *.beam

eldap_client.beam: eldap_client.erl
	erlc +debug eldap_client.erl

eldap_client: eldap_client.beam

tls_server.beam: tls_server.erl
	erlc +debug tls_server.erl

tls_server: tls_server.beam

tls_client.beam: tls_client.erl
	erlc +debug tls_client.erl

tls_client: tls_client.beam

run_eldap_client: eldap_client
	erl -noinput -s eldap_client start $(HOST) $(PORT) $(DN) $(PASSWD)

run_tls_client: tls_client
	erl -noinput -s tls_client start $(HOST) $(PORT)

run_tls_server: tls_server
	erl -noinput -s tls_server start $(PORT)

run_openssl_client:
	openssl s_client -connect $(HOST):$(PORT) \
		-CAfile $(CURDIR)/certs/ca_certificate.pem \
		-cert $(CURDIR)/certs/client_bakkenl-z01_certificate.pem \
		-key $(CURDIR)/certs/client_bakkenl-z01_key.pem \
		-verify 8 -verify_hostname $(HOST)

run_openssl_server:
	openssl s_server -accept $(PORT) \
		-CAfile $(CURDIR)/certs/ca_certificate.pem \
		-cert $(CURDIR)/certs/server_bakkenl-z01_certificate.pem \
		-key $(CURDIR)/certs/server_bakkenl-z01_key.pem
