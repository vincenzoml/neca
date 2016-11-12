
.PHONY: run clean

life.exe: life.fsx mypb.dll
	fsharpc -r mypb.dll life.fsx

mypb.dll: mypb.cs
	mcs -target:library -pkg:dotnet mypb.cs

run: life.exe
	mono life.exe

clean:
	rm -f mypb.dll life.exe *~ 
