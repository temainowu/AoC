file = open('pi0.txt', 'r')
array = [int(e) for e in file.readlines()]

tot = 0

for i in range(1, len(array)):
    tot += (array[i-1] < array[i])
print(tot)
