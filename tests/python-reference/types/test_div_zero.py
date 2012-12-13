try: 5.0 / 0.0
except ZeroDivisionError: pass
else: raise Exception("5.0 / 0.0 didn't raise ZeroDivisionError")

try: 5.0 // 0.0
except ZeroDivisionError: pass
else: raise Exception("5.0 // 0.0 didn't raise ZeroDivisionError")

try: 5.0 % 0.0
except ZeroDivisionError: pass
else: raise Exception("5.0 % 0.0 didn't raise ZeroDivisionError")

try: 5 / 0
except ZeroDivisionError: pass
else: raise Exception("5 / 0 didn't raise ZeroDivisionError")

try: 5 // 0
except ZeroDivisionError: pass
else: raise Exception("5 // 0 didn't raise ZeroDivisionError")

try: 5 % 0
except ZeroDivisionError: pass
else: raise Exception("5 % 0 didn't raise ZeroDivisionError")

