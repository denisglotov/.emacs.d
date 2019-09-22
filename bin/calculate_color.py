#!/usr/bin/env python3
import zlib

str = input()
data = zlib.adler32(str.encode())
r = data & 0x3F
data >>= 6
g = data & 0x7F
data >>= 7
b = data & 0x7F
print('#%2.2x%2.2x%2.2x' % (r, g, b))
