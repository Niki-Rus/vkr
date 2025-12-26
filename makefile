SRC = structure.lsp key-module.lsp encryption.lsp decryption.lsp tests.lsp

all:
	cat $(SRC) > /tmp/all.lsp
	sbcl --script /tmp/all.lsp

clean:
	rm -f /tmp/all.lsp


