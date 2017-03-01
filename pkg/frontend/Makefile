# This Makefile is cloned from the react-flux examples and does some
# post-processing on the `all.js` output of ghcjs.
#
# TODO:
#   - run closure from Build.hs in the git repo, and make it optional.
#   - see if the binding of global name spaces (global => window) is
#     necessary, or if we can find a better way to do that.
#   - why do we have to remove the `goog.{provide,require}` lines?

.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

# all: js-build/install-root js-build/frontend.min.js
all: js-build/install-root js-build/frontend.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/frontend.min.js: js-build/frontend.js
	closure --compilation_level=ADVANCED_OPTIMIZATIONS js-build/frontend.js > js-build/frontend.min.js

js-build/frontend.js: $(INSTALL_ROOT)/bin/refine-frontend.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/frontend.js-
	cat $(INSTALL_ROOT)/bin/refine-frontend.jsexe/all.js >> js-build/frontend.js-
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/frontend.js-
	sed -i 's/goog.provide.*//' js-build/frontend.js-
	sed -i 's/goog.require.*//' js-build/frontend.js-
	mv js-build/frontend.js- js-build/frontend.js

clean:
	rm -rf js-build
