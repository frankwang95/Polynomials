fh = open('out_15.txt', 'r')
n = 0
data = 1
while data != '':
    if n == 12680045:
        print data
    data = fh.readline()
    n += 1
print n

f1l = 71450624