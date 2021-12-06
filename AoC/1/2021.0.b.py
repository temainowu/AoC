file = open('pi0.txt', 'r')
array = [int(e) for e in file.readlines()]

for i in range(2, len(array)):
    array[i-2] += array[i-1] + array[i]

array.pop()
array.pop()

tot = 0
for i in range(1, len(array)):
    tot += (array[i-1] < array[i])
print(tot)
