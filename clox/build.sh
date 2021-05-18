#!/bin/sh

gcc main.c chunk.c memory.c debug.c value.c vm.c scanner.c compiler.c -o clox
