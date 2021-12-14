file = open('pi6.txt', 'r')
string = file.readlines()
string = str(string[0])
array = string.split(',')
array = [int(x) for x in array]
largest = max(array)
dist = [0]*largest
for i in range(largest):
    for j in range(len(array)):
        dist[i] += abs(array[j]-i)

'''
def search(xs, x):
    for k in range(len(xs)):
        if xs[k] == x:
            return k


print(search(dist, min(dist)))
'''

print(min(dist))
