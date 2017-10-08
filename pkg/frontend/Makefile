# This Makefile is cloned from the react-flux examples and does some
# post-processing on the `all.js` output of ghcjs.
#
# FUTUREWORK: merge with Build.hs

.PHONY: all clean
INSTALL_ROOT:=dist/build/refine-frontend
STACK_INSTALL_ROOT:=$(shell stack --stack-yaml=stack-js.yaml path --local-install-root)

# to build with cabal:
# curl https://www.stackage.org/lts-7.19/cabal.config >> cabal.config
# (uncomment a few entries in cabal.config if they conflict with the ghcjs constraints)
# cabal sandbox init
# cabal sandbox add-source ../{prelude,common} ~/src/{html-parse,react-flux{,-servant}}
# cabal install
#INSTALL_ROOT:=.cabal-sandbox


# VERY LONG BUILD-TIME: google-closure-compiler-js runs for >28
# minutes on a lenovo t420s with compilationLevel=WHITESPACE_ONLY.
# also consider https://prepack.io/ (discussed here:
# https://www.reddit.com/r/haskell/comments/698zl9/can_javascripts_prepack_help_with_ghcjs_output/)
GOOGLE_CLOSURE=time java -jar ./closure-compiler/closure-compiler-v20170626.jar --compilation_level=SIMPLE -W QUIET --third_party --js


# all: js-build/install-root js-build/frontend.min.js
all: js-build/frontend.js

optimize: all js-build/frontend.min.js
	cp js-build/frontend.min.js js-build/frontend.js
	touch js-build/frontend.min.js

setup-js-build: .phony
	mkdir -p js-build
	cp images/favicon.ico js-build/

js-build/install-root: setup-js-build $(INSTALL_ROOT)
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/frontend.min.js: js-build/frontend.js
	$(GOOGLE_CLOSURE) js-build/frontend.js > js-build/frontend.min.js
	ls -alF js-build/frontend.js js-build/frontend.min.js

js-build/frontend.js: js-build/install-root setup-js-build $(INSTALL_ROOT)/refine-frontend.jsexe/all.js
	echo "(function(global,React,ReactDOM) {" > js-build/frontend.js-
	cat $(INSTALL_ROOT)/refine-frontend.jsexe/all.js >> js-build/frontend.js-
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/frontend.js-
	sed -i 's/goog.provide.*//' js-build/frontend.js-
	sed -i 's/goog.require.*//' js-build/frontend.js-
	mv js-build/frontend.js- js-build/frontend.js

# build without nix.  see stack-js.yaml for why.
via-stack:
	mkdir -p js-build
	cp images/favicon.ico js-build/
	ln -sf $(STACK_INSTALL_ROOT) js-build/install-root
	stack build --stack-yaml=stack-js.yaml --flag refine-frontend:-ghc-O2 --ghc-options="-O0 -fno-ignore-asserts"
	echo "(function(global,React,ReactDOM) {" > js-build/frontend.js-
	cat $(STACK_INSTALL_ROOT)/bin/refine-frontend.jsexe/all.js >> js-build/frontend.js-
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/frontend.js-
	sed -i 's/goog.provide.*//' js-build/frontend.js-
	sed -i 's/goog.require.*//' js-build/frontend.js-
	mv js-build/frontend.js- js-build/frontend.js

clean:
	rm -rf js-build

.phony:
