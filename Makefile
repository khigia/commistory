BUILDDIR = _build

all:
	ghc \
		-hidir $(BUILDDIR) \
		-odir $(BUILDDIR) \
		-o commistory \
		--make Main.hs
