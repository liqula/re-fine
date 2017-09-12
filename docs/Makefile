# this is a collection of poorly documented make goals that serve
# their purpose under very specific (and also poorly documented)
# circumstances.  copy to ../ to run.  ask if you want more details!
# ~fisx


.phony:

sensei.%: .phony
	cd pkg/$* && stack exec -- sensei -Wall -Wno-unused-imports -I./src -i./src -i./test -i../prelude/src -i../prelude/test -I../common/src -i../common/src -i../common/test ./test/Spec.hs $(SENSEI_ARGS)

sensei.p: .phony
	make sensei.prelude

sensei.c: .phony
	make sensei.common

sensei.b: .phony
	./build build-backend || true
	make sensei.backend

seito.p: .phony
	make -C pkg/prelude seito 2>&1 | perl -ne '/(.{0,300})/ && print "$$1\n"'

seito.c: .phony
	make -C pkg/common seito 2>&1 | perl -ne '/^(.{0,300})/ && print "$$1\n"'

seito.b: .phony
	make -C pkg/backend seito 2>&1 | perl -ne '/^(.{0,300})/ && print "$$1\n"'

seito.f: .phony
	make -C pkg/frontend seito 2>&1 | perl -ne '/^(.{0,300})/ && print "$$1\n"'

grepi.%: .phony
	git grep -Hni $*

grep.%: .phony
	git grep -Hn $*

# http://hackage.haskell.org/packages/search?terms=tags
# https://wiki.haskell.org/Tags
#
# - ghc built-in ctags creation
#
# - http://hackage.haskell.org/package/hothasktags
#   doesn't build straight from hackage with stack; has parse errors
#
# - http://hackage.haskell.org/package/htags only parses haskell98.
#
# - http://hackage.haskell.org/package/hasktags,
#   http://hackage.haskell.org/package/fast-tags,
#   http://hackage.haskell.org/package/haskdogs all use their own parser
#   and do not distinguish case (`values` vs. `Types`).
#
# - http://hackage.haskell.org/package/lushtags
#   better vim integration
#
# - https://kingfisher.nfshost.com/sw/gasbag/
#   not on hackage, seems poorly supported.
#
# - http://hackage.haskell.org/package/textmatetags
#   (i don't think this has anything to do with that kind of tags)
tags: .phony
	hasktags -b pkg
#	echo ':etags' | stack --stack-yaml=stack-no-js.yaml exec -- ghci -isrc -itest -i../common/src test/AllModules.hs
#	# then how do you merge these?  do i have to merge?  yes, i
#	# have to merge backend and frontend.  or switch.  i can write
#	# emacs shortcuts to switch between frontend and backend (both
#	# will pull in common, prelude)
#	find pkg -name '*.hs' | xargs hothasktags > tags

test: .phony
	./build

hlint: .phony
	./build hlint

sensei.f: .phony
#	cd pkg/frontend && stack --stack-yaml=stack-no-js.yaml test --fast
	cd pkg/frontend && stack --stack-yaml=stack-no-js.yaml exec -- sensei -Werror -Wall -Wno-unused-imports -Isrc -isrc -itest -I../common/src -i../common/src -i../prelude/src test/Main.hs --match=###

sensei.fs:
	cd pkg/frontend && stack --stack-yaml=stack-no-js.yaml exec -- sensei -Isrc -isrc -itest -I../common/src -i../common/src -i../prelude/src styleguide/Main.hs --match=###


test.f: .phony
	( cd pkg/frontend && time stack test --fast --flag refine-frontend:-ghc-warn-error --test-arguments "$(SENSEI_ARGS)" ) || ( make noise-nay; false )
	make noise-yeay

debug.f:
	@echo
	@echo
	@echo 'use `debugger;` in your js code to stop execution.'
	@echo 'see also: https://medium.com/@paul_irish/debugging-node-js-nightlies-with-chrome-devtools-7c4a1b95ae27'
	@echo
	@echo
	( cd pkg/frontend && stack test --fast --flag refine-frontend:-ghc-warn-error --test-arguments "--match=#####" ) || true
	( cd pkg/frontend && node --inspect --debug-brk `stack path --dist-dir`/build/spec/spec.jsexe/all.js --match=### )

mk.f: .phony
	( cd pkg/frontend && time stack build --fast --flag refine-frontend:-ghc-warn-error --no-test && make ) || ( make noise-nay; false )
	make noise-yeay

noise-yeay: .phony
	mplayer --volume 100 /mnt/X/home/mf/backup/attic/home1/docs/snd/sounds_wavs/shiph.ogg

noise-nay: .phony
	mplayer --volume 100 /mnt/X/home/mf/backup/attic/home1/docs/snd/sounds_wavs/winny.ogg

clean: .phony
	find . -name '*~' -exec rm -f {} \;


# weed out dead library dependencies.  (copied from thentos; not tested.)

build-packunused: .phony
	cabal get packunused || (echo "rm stale copy of packunused?"; false)
	cd packunused-* && \
	cabal sandbox init --sandbox=../.cabal-sandbox && \
	cabal install packunused --constraint="Cabal==`cabal --version | perl -ne '/using version (.*) of the Cabal library/ && print $$1'`"

%.packunused: .phony
	@echo
	@echo '| make build-packunused first to ensure freshness of executable.'
	@echo '| (See https://github.com/hvr/packunused/issues/5.)'
	@echo '|'
	@echo '| WARNING: packunused on hspec-discover tests cannot see Spec modules'
	@echo '|          and will issue spurious warnings about unused libs!'
	@echo

	cd $* && \
	../.cabal-sandbox/bin/packunused --help >/dev/null && \
	cabal clean && \
	rm -f *.imports && \
	cabal configure -O0 --disable-library-profiling --enable-test --enable-bench && \
	cabal build --ghc-option=-ddump-minimal-imports && \
	../.cabal-sandbox/bin/packunused

packunused: .phony thentos-core.packunused thentos-tests.packunused thentos-adhocracy.packunused


PACKAGES=refine-backend persistent persistent-sqlite persistent-template users-persistent esqueleto

super-clean:
	cd pkg/backend; for i in `echo $(PACKAGES)`; do stack exec -- ghc-pkg unregister $$i; done

run-server:
	cd pkg/backend && \
	stack build --fast && \
	rm -rf .backend-data/ && \
	stack exec refine -- --init sample-content.yaml server.conf && \
	stack exec refine -- server.conf
