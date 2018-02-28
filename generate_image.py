import multiprocessing as mp
import numpy as np
import math
from PIL import Image


pixels = 25000
image_range = np.array([[-2.0, 2.0], [-2.0, 2.0]])
output_filename = 'image.tif'
total_roots = 4000000000
chunk_size = 100000
n_processors = 32
n_jobs = 128


def generate_polynomial(n):
    code = np.base_repr(n, 3)
    return np.array([int(s) - 1 for s in code])

def solve_polynomial(work_range):
    start_n, stop_n = work_range
    roots = np.array([])
    for n in range(start_n, stop_n):
        poly = generate_polynomial(n)
        if not np.all(poly == 0):
            roots = np.concatenate((roots, np.roots(poly)))
    return np.stack((roots.real, roots.imag), -1)

def generate_hist(data):
    return np.histogram2d(
        data[:, 1], data[:, 0],
        range=image_range, bins=pixels
    )[0]

def main():
    n = 0
    histogram = np.zeros((pixels, pixels))
    worker_pool = mp.Pool(n_processors)

    while True:
        lower_range = chunk_size * n
        upper_range = chunk_size * (n + 1)
        incr = math.ceil((upper_range - lower_range) / n_jobs)

        print('\rworking on range: {}, {}'.format(lower_range, upper_range), end='')
        if lower_range >= total_roots: break

        jobs = [
            (lower_range + incr * l, min(lower_range + incr * (l + 1), upper_range))
        for l in range(n_jobs)]

        roots = worker_pool.map(solve_polynomial, jobs)
        roots = np.concatenate(roots)
        histogram += generate_hist(roots)
        n += 1

    print('\nFinished generating historam, writing image...')
    histogram = histogram * 4
    histogram = np.clip(np.log(histogram + 1) * 2 / np.log(histogram.max() + 1), 0, 1)
    histogram = (np.stack((histogram, histogram, histogram), axis=2) * 255).astype('uint8')
    im = Image.fromarray(histogram)
    im.save(output_filename)
    print('..done.')

main()
