import sys
from time import sleep


if __name__ == "__main__":
    wait_time = int(sys.argv[1]) if len(sys.argv) > 1 else 10

    for i in range(wait_time):
        if i & 1 == 0:
            print("tick")
        else:
            print("tock")
        sleep(1.0)

    print("DING!")
