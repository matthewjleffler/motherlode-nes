import sys
import math

print("Given a speed value and a number of rotations, create a constant table describing thsoe rotations.")
if len(sys.argv) != 4:
    print("Supply arguments: name, speed, num rotations")
    exit()

name = sys.argv[1]
speed = float(sys.argv[2])
rotations = int(sys.argv[3])
rad = (math.pi * 2) / rotations

resultsubx = '  .db '
resultx = '  .db '
resultsuby = '  .db '
resulty = '  .db '


def get_vals(raw):
    if (raw < 0):
        rawpixel = math.ceil(raw)
        pixel = abs(rawpixel)
        pixel = 128 + pixel
    else:
        rawpixel = math.floor(raw)
        pixel = rawpixel
    pixelhex = '{:02X}'.format(int(pixel))
    subhex = '{:02X}'.format(int(round(abs(raw - rawpixel) * 255)))
    return (pixelhex, subhex)


for i in range(0, rotations):
    currentrad = rad * i
    x = round(math.cos(currentrad) * speed, 2)
    y = round(math.sin(currentrad) * speed, 2)

    if (i > 0):
        resultsubx += ','
        resultx += ','
        resultsuby += ','
        resulty += ','

    vals = get_vals(x)
    resultsubx += '${}'.format(vals[1])
    resultx += '${}'.format(vals[0])

    vals = get_vals(y)
    resultsuby += '${}'.format(vals[1])
    resulty += '${}'.format(vals[0])

print('')
print(name + 'X:')
print(resultsubx)
print(resultx)
print(name + 'Y:')
print(resultsuby)
print(resulty)
print('')
