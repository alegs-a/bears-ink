import sys
import textwrap
import pathlib

from PIL import Image

if len(sys.argv) < 2:
    print(f'{sys.argv[0]} <file.png>')

_, filename = sys.argv

filepath = pathlib.Path(filename)

image = None
try:
    image = Image.open(filepath)
except Exception as err:
    print(f'Failed to open PNG file at {filepath}: {err}')
    exit(1)

x, y = image.size

if y % 8 != 0:
    print('Too little rows.')
    print(f'size is ({x}, {y})')
    exit(1)

rows = y // 8

pixels = list(image.getdata().convert('1'))

buffer = []

for row in range(rows):
    row_offset = 8 * row * x

    for col in range(x):
        col_offset = col

        byte = 0
        for pixel in range(8):
            pixel_offset = pixel * x

            bit = pixels[row_offset + col_offset + pixel_offset] > 0
            byte |= bit << pixel

        buffer.append(byte)

code = '\n'.join((
    'unsigned char buffer_{filename}[{size}] = {{',
    '{bytes}',
    '}};',
    '',
    'struct Image image_{filename} = {{',
    '    .width = {width},',
    '    .height = {height},',
    '    .size = {size},',
    '    .buffer = buffer_{filename}',
    '}};'
)).format(
    filename = filepath.stem,
    size = len(buffer),
    bytes = textwrap.indent(
        '\n'.join(textwrap.wrap(', '.join([f'0x{x:02X}' for x in buffer]) , 76)),
        ' ' * 4
    ), 
    width = x - 1,
    height = rows
)

print()
print(code)
