
# zero, uma ou várias definições de funções no início do ficheiro
def fibaux(a, b, k):
    if k == 0:
        return a
    else:
        return fibaux(b, a+b, k-1)

def fib(n):
    return fibaux(0, 1, n)

# uma ou várias instruções no final do ficheiro
print("alguns valores da sequência de Fibonacci :")
for n in [0, 1, 11, 42]:
    print(fib(n))

