file = open('2022/pi0.txt', 'r')
array = file.readlines()

tot = 0
x = []

for i in range(len(array)):
    if array[i] == '\n':
        x.append(tot)
        tot = 0
    else:
        tot += int(array[i])
x.append(tot)
print(max(x))