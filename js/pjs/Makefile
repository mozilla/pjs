SRC=./src
BUILD=./build
INSTALL=./build/install
MOZCENTRAL=./mozilla-central
OBJS =											\
	${BUILD}/pjs.o								\
	${BUILD}/main.o								\
	${BUILD}/membrane.o

.PHONY : all TAGS clean cleaner

all: ${BUILD}/pjs

.MC.loc:
	./checkout-mozilla-central

${BUILD}/MC: .MC.loc
	mkdir -p ${BUILD}
	./build-mozilla-central

# Add mozilla-central to our include headers so that
# we can include non-public headers like "js/src/jsobj.h"
${BUILD}/%.o: ${SRC}/%.cpp ${BUILD}/MC
	g++ -DDEBUG -O0 -g -w -m64 -fno-rtti			\
		$$(${BUILD}/install/bin/js-config --cflags)	\
		-I $$(cat .MC.loc)							\
		-o $@ -c $<

# Use static linking rather than dynaminc linking
# so that we have access to the non-exported symbols.
#		$$(${BUILD}/install/bin/js-config --libs)
${BUILD}/pjs: ${OBJS} ${BUILD}/MC
	g++ -m64									\
		${BUILD}/install/lib/libjs_static.a		\
		${BUILD}/install/lib/libnspr4.a			\
		-o $@ ${OBJS}

TAGS: .MC.loc
	ctags -e -f TAGS.emacs -R ${SRC}			\
		$$(cat .MC.loc)/js						\
		$$(cat .MC.loc)/nsprpub					\
		$$(cat .MC.loc)/mfbt

clean:
	rm -rf ${BUILD}
	rm -f TAGS.emacs

cleaner: clean
	rm -rf mozilla-central .MC.loc
