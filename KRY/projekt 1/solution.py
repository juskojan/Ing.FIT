# KRY 1 
# Jan Jusko (xjusko00@stud.fit.vutbr.cz)
# 13.04.2018

import sys
import os.path

# Static global vars
SUB = [0, 1, 1, 0, 1, 0, 1, 0]
N_B = 32
N = 32 * 8

# Access files
def readFromFile(filePath):
    if os.path.isfile(filePath):
        return open(filePath, 'rb').read(N_B)
    else:
        sys.stderr.write('File ' + filePath + ' not found!')
        return -1

# Returns values according to SUB array
def getValues(LSBY, MSBX, value, bit):
    nextValues = []
    for i in range(len(SUB)):
        lowI = i & 0x3
        if SUB[i] == LSBY and lowI == MSBX:
            value |= (i << bit)
            nextValues.append(value)
    return nextValues

def reverseX(values):
    for value in values:
        if (value >> N) == (value & 0x3):
            # Only N bits
            mask = (0x1 << N)
            reversedX = (value >> 1) & ~mask
            return reversedX
    return NULL

# Previous keystream
def reversedStep(value):
    # Get values according to indexes
    values = []
    for i in range(len(SUB)):
        if SUB[i] == value & 0x1:
            values.append(i)

    for position in range(1, N):
        nextValues = []
        ends = {'Y': (value >> position) & 0x1}
        for val in values:
            ends['X'] = (val >> position) & 0x3
            nextValues.extend(getValues(ends['Y'], ends['X'], val, position))
        values = nextValues
    return reverseX(nextValues)

# Main function
if __name__ == '__main__':
    # Load files
    plainText = readFromFile('in/bis.txt')
    cipherText = readFromFile('in/bis.txt.enc')
    if (plainText == -1) or (cipherText == -1):
        sys.exit(-1)

    # Initial key!
    key = int.from_bytes(plainText, 'little') ^ int.from_bytes(cipherText, 'little')

    # Cipher cycles
    for i in range(N // 2):
        key = reversedStep(key)

    print(key.to_bytes(N_B, 'little').decode())
