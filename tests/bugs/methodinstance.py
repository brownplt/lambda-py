class F:
    def get5(self):
        return 5

f = F()
call = f.get5  # here call has __func__
call = call.__call__
# __func__ only exists in method instance
# call now is not a method instance
___assertFalse(hasattr(call, '__func__'))
