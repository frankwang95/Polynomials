from itertools import islice
import numpy as np
from PIL import Image


n = 5000000
input_filename = 'out.txt'
image_range = np.array([[-2.0, 2.0], [-2.0, 2.0]])
pixels = 15000
output_filename = 'image.tif'


def generate_hist(data):
    return np.histogram2d(
        data[:, 1], data[:, 0],
        range=image_range, bins=pixels
    )[0]

def parse_to_numbers(chunk):
    roots = []
    for c in chunk:
        # Ignoring weird mathematica scientic notation output
        try:
            roots.append([float(s) for s in c.split(',')])
        except:
            continue
    return np.array(roots)

def main():
    with open(input_filename, 'r') as f:
        histogram = np.zeros((pixels, pixels))
        i = 0
        while True:
            print('\rdone: {}'.format(i * n), end='')
            chunk = list(islice(f, n))
            if not chunk: break
            data = parse_to_numbers(chunk)
            histogram += generate_hist(data)
            i += 1
    print('\n')
    print('Finished generating historam, writing image...')

    histogram = histogram * 4
    histogram = np.clip(np.log(histogram + 1) * 2 / np.log(histogram.max() + 1), 0, 1)
    histogram = (np.stack((histogram, histogram, histogram), axis=2) * 255).astype('uint8')
    im = Image.fromarray(histogram)
    im.save(output_filename)


main()
