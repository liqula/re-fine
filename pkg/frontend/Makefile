# This Makefile is cloned from the react-flux examples and does some
# post-processing on the `all.js` output of ghcjs.
#
# FUTUREWORK: merge with Build.hs

.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

# VERY LONG BUILD-TIME: google-closure-compiler-js runs for >28
# minutes on a lenovo t420s with compilationLevel=WHITESPACE_ONLY.
GOOGLE_CLOSURE=time ./node_modules/.bin/google-closure-compiler-js --compilationLevel=ADVANCED

# all: js-build/install-root js-build/frontend.min.js
all: js-build/install-root js-build/frontend.js

optimize: all js-build/frontend.min.js
	cp js-build/frontend.min.js js-build/frontend.js
	touch js-build/frontend.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/frontend.min.js: js-build/frontend.js
	$(GOOGLE_CLOSURE) js-build/frontend.js > js-build/frontend.min.js
	ls -alF js-build/frontend.js js-build/frontend.min.js

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
