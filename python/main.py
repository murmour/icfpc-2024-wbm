import subprocess, io, os
import time


def interact(request):
    return subprocess.check_output(['../kepler/interact', '-ss', request])


def download(request, path, n):
    for i in range(1, n+1):
        input_file = '%s/%02d.in' % (path, i)
        if os.path.isfile(input_file):
            print('Skip %d' %i)
            continue
        print('%d...' % i)
        data = interact(request % i)
        print(data)
        with io.open(input_file, 'wb') as f:
            f.write(data)
        time.sleep(4)


def download_lambdaman():
    download("get lambdaman%d", "../../data/lambdaman", 21)


def download_spaceship():
    download("get spaceship%d", "../../data/spaceship", 25)


def download_3d():
    download("get 3d%d", "../../data/3d", 12)

def download_efficiency():
    download("get efficiency%d", "../../data/efficency", 13)


def create_folders(task_name, cnt):
    for i in range(cnt):
        n = i + 1
        fname = '../../data/out/%s/%02d' % (task_name, n)
        os.makedirs(fname, exist_ok=True)


def main():
    #download_lambdaman()
    #download_spaceship()
    #download_3d()
    download_efficiency()
    #create_folders("spaceship", 25)
    #create_folders("lambdaman", 21)


if __name__ == "__main__":
    main()
