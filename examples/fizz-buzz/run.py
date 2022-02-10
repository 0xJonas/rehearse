import sys;


def fizz_buzz(num):
    if num % 15 == 0:
        return "fizzbuzz"
    elif num % 5 == 0:
        return "buzz"
    elif num % 3 == 0:
        return "fizz"
    else:
        return str(num)


if __name__ == "__main__":
    stop = int(sys.argv[1]) if len(sys.argv) > 1 else 20
    for i in range(1, stop + 1):
        print(fizz_buzz(i))
