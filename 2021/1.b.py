file = open('pi1.txt', 'r')
array = file.readlines()

dist = 0
aim = 0
dep0 = 0

for i in array:
    if i[0] == 'f':
        dist += int(i[8])
        dep0 += (aim * int(i[8]))
    elif i[0] == 'd':
        aim += int(i[5])
    elif i[0] == 'u':
        aim -= int(i[3])

print(dist * dep0)
