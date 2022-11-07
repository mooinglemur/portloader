ca65 portloader.s -D X16 -g -o portloader.o
ld65 -o PORTLOADER.PRG -C portloader.cfg -m portloader-map.txt -Ln portloader.sym portloader.o
