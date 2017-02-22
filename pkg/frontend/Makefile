.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

# all: js-build/install-root js-build/frontend.min.js
all: js-build/install-root js-build/frontend.js
# all: js-build/install-root js-build/lib.js js-build/out.js js-build/rts.js

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

js-build/lib.js: $(INSTALL_ROOT)/bin/refine-frontend.jsexe/lib.js
	mkdir -p js-build
	echo "(function(React) {" > js-build/lib.js-
	cat $(INSTALL_ROOT)/bin/refine-frontend.jsexe/lib.js >> js-build/lib.js-
	echo "})(window['React']);" >> js-build/lib.js-
	sed -i 's/goog.provide.*//' js-build/lib.js-
	sed -i 's/goog.require.*//' js-build/lib.js-
	mv js-build/lib.js- js-build/lib.js

js-build/out.js: $(INSTALL_ROOT)/bin/refine-frontend.jsexe/out.js
	mkdir -p js-build
	echo "(function(React,ReactDOM) {" > js-build/out.js-
	cat $(INSTALL_ROOT)/bin/refine-frontend.jsexe/out.js >> js-build/out.js-
	echo "})(window['React'], window['ReactDOM']);" >> js-build/out.js-
	mv js-build/out.js- js-build/out.js

js-build/rts.js: $(INSTALL_ROOT)/bin/refine-frontend.jsexe/rts.js
	mkdir -p js-build
	echo "(function(global) {" > js-build/rts.js-
	cat $(INSTALL_ROOT)/bin/refine-frontend.jsexe/rts.js >> js-build/rts.js-
	echo "})(window);" >> js-build/rts.js-
	sed -i 's/goog.provide.*//' js-build/rts.js-
	sed -i 's/goog.require.*//' js-build/rts.js-
	mv js-build/rts.js- js-build/rts.js


clean:
	rm -rf js-build
