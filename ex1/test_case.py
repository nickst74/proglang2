import random
import sys
from collections import deque
import numpy as np


# just a python program for generating random inputs
n = int(sys.argv[1])
print(n)
root = 1
buffer = {root : 0}
dq = deque()
dq.append(root)
n = n - 1
current = 2
while n :
    element = dq.popleft()
    number = random.randint(0,min(100,n))
    for i in range(number):
        buffer[current] = element
        dq.append(current)
        current+=1
    n -= number

for v in np.random.permutation(list(buffer.values())) :
    print(1, v)
    #print(random.randint(1, 100), v)
