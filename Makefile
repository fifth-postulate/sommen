.phony: all, image, clean

SOURCE_DIR=src
ASSETS_DIR=docs
JS_DIR=${ASSETS_DIR}/js

all: ${JS_DIR}/Sommen.min.js ${JS_DIR}/Sommen.debug.js ${JS_DIR}/Admin.min.js ${ASSETS_DIR}/debug.html 
	@echo "finished building frontend application"

${JS_DIR}/%.min.js: ${JS_DIR}/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $@

${JS_DIR}/%.debug.js: ${SOURCE_DIR}/%.elm ${SOURCE_DIR}/*.elm
	elm make $< --debug --output=$@

${JS_DIR}/%.js: ${SOURCE_DIR}/%.elm ${SOURCE_DIR}/*.elm
	elm make $< --optimize --output=$@

${ASSETS_DIR}/debug.html: ${ASSETS_DIR}/index.html
	sed 's/Sommen.min.js/Sommen.debug.js/' $< > $@

clean:
	rm -f ${JS_DIR}/{Sommen,Admin}.min.js ${JS_DIR}/Sommen.debug.js