#!/usr/bin/env python3
def dot(r, g, b):
    print(f'\033[48;2;{r};{g};{b}m' f'\033[38;2;{255-r};{255-g};{255-b}m' 'X', end='')
for f in range(127):
    dot(f, 2*f, 255-f)
print('\033[0m')
