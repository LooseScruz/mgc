#!/bin/bash
llvm-as-6.0 ll
chmod +x ll.bc
lli-6.0 ./ll.bc