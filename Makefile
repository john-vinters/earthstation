
FLAGS	= -O3 -gnatn -gnatN -j2 -gnatwa

default: earthstation

dirs:
	@mkdir -p build

earthstation: dirs
	gnatmake $(FLAGS) -gnat05 -Asrc -D build es.adb `gtkada-config`

clean:
	rm -f build/*
	rm -f es

