#!/usr/bin/env python3
def dot(r, g, b):
    print(f'\033[48;2;{r};{g};{b}m' f'\033[38;2;{255-r};{255-g};{255-b}m' 'X', end='')
    #print(r, g, b)
def sabs(x):
    det = (int(x/256))%2
    return 255 * det + x%256 * (1-det*2)
for f in range(1024):
    dot(sabs(f), sabs(2*f), sabs(1024+255-f))
print('\033[0m')
