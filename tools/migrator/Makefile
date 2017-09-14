DEB_MIGRATOR := $(NAME)-migrator_$(VERSION)+$(BUILD)_amd64.deb

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: all

all: clean install

init:
	mkdir -p dist

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: install
install: init
	stack install --pedantic --test --local-bin-path=dist

.PHONY:
compile:
	stack build --pedantic --test --no-copy-bins

.PHONY: dist
dist: guard-VERSION install $(DEB_JOURNALER) .metadata

$(DEB_MIGRATOR):
	makedeb --name=$(NAME)-migrator \
	 --version=$(VERSION) \
	 --debian-dir=deb-migrator \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata
