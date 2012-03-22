SRC=./src
BUILD=./build
INSTALL=./build/install
MC=../..
OBJS =						\
	${BUILD}/pjs.o				\
	${BUILD}/main.o				\
	${BUILD}/membrane.o
HEADERS=${wildcard ${SRC}/*.h}

.PHONY : all TAGS clean cleaner test

all: ${BUILD}/pjs

${MC}/js/src/configure:
	cd ${MC}/js/src && autoconf-2.13

${BUILD}/MC: ${MC}/js/src/configure
	mkdir -p ${BUILD}
	./build-mozilla-central

# Add mozilla-central/js/src to our include headers so that
# we can include non-public headers like "jsobj.h"
${BUILD}/%.o: ${SRC}/%.cpp ${HEADERS} ${BUILD}/MC
	g++ -DDEBUG -O0 -g -w -m64 -fno-rtti			\
		$$(${BUILD}/install/bin/js-config --cflags)	\
		-I ${MC}/js/src								\
		-o $@ -c $<

# Use static linking rather than dynamicn linking
# so that we have access to the non-exported symbols.
#		$$(${BUILD}/install/bin/js-config --libs)
${BUILD}/pjs: ${OBJS} ${BUILD}/MC
	g++ -m64					\
		${BUILD}/install/lib/libjs_static.a	\
		${BUILD}/install/lib/libnspr4.a		\
		-o $@ ${OBJS}

TAGS:
	ctags -e -f TAGS.emacs -R ${SRC}	\
		$(MC)/js			\
		$(MC)/nsprpub			\
		$(MC)/mfbt

clean:
	rm -rf ${BUILD}
	rm -f TAGS.emacs

test: ${BUILD}/pjs
	./run-test test/*.js