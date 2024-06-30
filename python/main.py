import subprocess, io, os
import time


def interact(request):
    return subprocess.check_output(['../../data/ocaml_bins/interact', '-ss', 'stdin'], input=request.encode())


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
    download("get lambdaman%d", "../../data/in/lambdaman", 21)


def download_spaceship():
    download("get spaceship%d", "../../data/spaceship", 25)


def download_3d():
    download("get 3d%d", "../../data/3d", 12)


def create_folders(task_name, cnt):
    for i in range(cnt):
        n = i + 1
        fname = '../../data/out/%s/%02d' % (task_name, n)
        os.makedirs(fname, exist_ok=True)

def submit(task, pid, tag):
    tag = tag.replace('<pid>', '%02d' % pid)
    fname = '../../data/out/%s/%02d/%s' % (task, pid, tag)
    with io.open(fname) as f:
        data = f.read()
    data = 'solve spaceship%d ' % pid + data.split()[-1]
    res = interact(data)
    print(res)

def submit_spaceship(pid, tag="first"):
    submit("spaceship", pid, tag)

def submit_all():
    for i in range(2,23):
        submit_spaceship(i, "spaceship<pid>_sa_t60")
        time.sleep(4)

def print_point_counts():
    n_tests = 25
    for i in range(1, n_tests + 1):
        fname = '../../data/in/spaceship/%02d.in' % i
        with io.open(fname, 'rt') as f:
            n = len(f.read().strip().splitlines())
            print('%d: %d' % (i, n))



def main():
    download_lambdaman()
    #download_spaceship()
    #download_3d()
    #create_folders("spaceship", 25)
    #create_folders("lambdaman", 21)
    #submit_spaceship(13)
    #submit_spaceship(12, "naive.sol")
    #submit_all()
    #print_point_counts()


if __name__ == "__main__":
    main()
