from sys import argv

def usage():
    print("python3 asm.py <path>")

def main():
    if len(argv) < 2:
        usage()
        exit()
    
if __name__ == '__main__':
    main()