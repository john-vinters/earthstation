
FLAGS	= -O3 -gnatn -gnatN -j2 -gnatwa

earthstation:
	gnatmake $(FLAGS) -gnat05 -Asrc -D build es.adb `gtkada-config`

clean:
	rm -f build/*
	rm -f es

