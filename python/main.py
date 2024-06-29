import subprocess, io, os
import time

def interact(request):
    return subprocess.check_output(['../../data/ocaml_bins/interact', '-ss', request])

def download_lambdaman():
    for i in range(1, 22):
        input_file = '../../data/lambdaman/%02d.in' % i
        if os.path.isfile(input_file):
            print('Skip %d' % i)
            continue
        print('%d...' % i)
        request = 'get lambdaman%d' % i
        data = interact(request)
        print(data)
        with io.open(input_file, 'wb') as f:
            f.write(data)
        time.sleep(4)

def download_spaceship():
    for i in range(1, 26):
        input_file = '../../data/spaceship/%02d.in' % i
        if os.path.isfile(input_file):
            print('Skip %d' % i)
            continue
        print('%d...' % i)
        request = 'get spaceship%d' % i
        data = interact(request)
        print(data)
        with io.open(input_file, 'wb') as f:
            f.write(data)
        time.sleep(4)

def create_folders(task_name, cnt):
    for i in range(cnt):
        n = i + 1
        fname = '../../data/out/%s/%02d' % (task_name, n)
        os.makedirs(fname, exist_ok=True)

def main():
    #download_lambdaman()
    #download_spaceship()
    #create_folders("spaceship", 25)
    create_folders("lambdaman", 21)

main()