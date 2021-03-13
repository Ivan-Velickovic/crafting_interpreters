all: run

compile:
	CLASSPATH=../ javac Lox.java

clean:
	find . -iname '*.class' -delete

run: clean compile
	CLASSPATH=../ java lox.Lox

.PHONY: all clean run
