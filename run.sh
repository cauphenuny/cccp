#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
    echo "usage: $0 <source-file>"
    exit 1
fi
build="cmake -Bbuild && make -C build"
name=${1%.*}
compile="build/compiler -riscv $name.c -o $name.S"
asm="clang $name.S -c -o $name.o -target riscv32-unknown-linux-elf -march=rv32im -mabi=ilp32"
link="ld.lld $name.o -L$CDE_LIBRARY_PATH/riscv32 -lsysy -o $name"
run="qemu-riscv32-static $name"

echo $build && eval $build && 
echo $compile && eval $compile && 
echo $asm && eval $asm && 
echo $link && eval $link && 
echo $run && eval $run
