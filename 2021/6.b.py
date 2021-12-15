file = open('pi6.txt', 'r')
string = file.readlines()
string = str(string[0])
array = string.split(',')
array = [int(x) for x in array]
largest = max(array)
fuel = [0.0]*largest
for i in range(largest):
    for j in range(len(array)):
        x = abs(array[j]-i)
        fuel[i] += ((x*(x+1))/2)

print(min(fuel))
