# Convert a PNG image to a flattened text representation with RGB triplets for
# each data point.  Don't forget to insert the number of clusters k_max into
# the 1st line before feeding it to the program as the *.in file.
import sys
import numpy as np
from pathlib import Path
from PIL import Image

args = sys.argv[1:]
if not args:
    print("Usage:\n\tpython3 png2txt.py <image.png>")
    sys.exit(1)
png_filename = args[0]
txt_filename = Path(png_filename).stem + '.txt'

im_frame = Image.open(png_filename)
img = np.array(im_frame)
nX, nY, nC = img.shape

with open(txt_filename, 'w') as f:
    for i in range(nX):
        for j in range(nY):
            f.write("{:3d} {:3d} {:3d}\n".format(*img[i, j, :]))
