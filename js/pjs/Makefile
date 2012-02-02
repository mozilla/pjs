SRC=./src
BUILD=./build
INSTALL=./build/install
MC=../..
OBJS =						\
	${BUILD}/pjs.o				\
	${BUILD}/main.o				\
	${BUILD}/membrane.o

.PHONY : all TAGS clean cleaner

all: ${BUILD}/pjs

${MC}/js/src/configure:
	cd ${MC}/js/src && autoconf213

${BUILD}/MC: ${MC}/js/src/configure
	mkdir -p ${BUILD}
	./build-mozilla-central

# Add mozilla-central to our include headers so that
# we can include non-public headers like "js/src/jsobj.h"
${BUILD}/%.o: ${SRC}/%.cpp ${BUILD}/MC
	g++ -DDEBUG -O0 -g -w -m64 -fno-rtti			\
		$$(${BUILD}/install/bin/js-config --cflags)	\
		-I ${MC}									\
		-o $@ -c $<

# Use static linking rather than dynaminc linking
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
