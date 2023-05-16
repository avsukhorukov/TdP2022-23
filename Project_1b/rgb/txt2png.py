# Specify the old PNG image and the txt file with quantized data points replaced
# by centroids of the clusters.  It will create the quantized PNG image.
import sys
import numpy as np
from pathlib import Path
from PIL import Image

args = sys.argv[1:]
if not args:
    print("Usage:\n\tpython3 txt2png.py <old_image.png> <file.txt>")
    sys.exit(1)
old_png_filename = args[0]
txt_filename = args[1]
new_png_filename = Path(txt_filename).stem + '_quantized.png'

# Get the original dimensions from the old image.
with Image.open(old_png_filename) as im_frame:
    img = np.array(im_frame)
    nX, nY, nC = img.shape

with open(txt_filename, "r") as f:
    img = np.empty((nX, nY, nC), dtype=np.uint8)
    for i in range(nX):
        for j in range(nY):
            R, G, B = [int(w) for w in f.readline().split()]
            img[i, j, :] = [R, G, B]

im = Image.fromarray(img)
im.save(new_png_filename)