file = open('pi2.txt', 'r')
array = file.readlines()
array = [G.strip() for G in array]
array2 = ['']*12

for i, _ in enumerate(array2):
    for x in array:
        array2[i] += x[i]


def sum(xs):
    total = 0
    for x in xs:
        total += int(x)
    return total


sums = [sum(x) >= 500 for x in array2]
print(str(sums))
