s <- String("Hello world!")

print(toLowerCase(s))        # "hello world!"
print(toUpperCase(s))        # "HELLO WORLD!"

print(substring(s,0, 5))      # "Hello"
print(substring(s,6))         # "world!"

print(indexOf(s,"w"))         # 6
print(lastIndexOf(s,"o"))     # 7

print(startsWith(s,"Hello"))  # TRUE
print(endsWith(s,"world"))    # FALSE

print(String$valueOf(156))    # "156"

print(charAt(s,0))            # "H"
print(charAt(s,9))            # "l"
\dontrun{print(charAt(s,14))}           # Error: index of out bounds.

