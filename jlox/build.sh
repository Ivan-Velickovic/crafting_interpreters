#!/bin/sh

find . -iname '*.class' -delete
# AST gen
CLASSPATH=. javac tool/GenerateAst.java
CLASSPATH=. java tool.GenerateAst lox/

# Build interpreter
CLASSPATH=. javac lox/Lox.java

