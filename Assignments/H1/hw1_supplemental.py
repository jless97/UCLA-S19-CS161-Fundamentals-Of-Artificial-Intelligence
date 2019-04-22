''' Fibonacci Sequence '''

# Recursive
def fib(n):
  if (n <= 1):
    return n

  return fib(n - 1) + fib(n - 2)

# DP
def fib_dp(n):
  dp = [0 for i in range(n + 1)]
  dp[0], dp[1] = 0, 1
  for i in range(2, n + 1):
    dp[i] = dp[i - 1] + dp[i - 2]
    print("i: {}, dp[i - 2]: {}, dp[i - 1]: {}, dp[i]: {}".format(i, dp[i - 2], dp[i - 1], dp[i]))

  return dp[-1]

# DP w/ constant space
def fib_dp_constant_space(n):
  if (n == 0):
    return 0
  if (n == 1):
    return 1

  first, second = 0, 1
  for i in range(2, n + 1):
    third = first + second
    first = second
    second = third

  return third

''' Padovan Sequence '''

# Recursive
def pad(n):
  if (n == 0 or n == 1 or n == 2):
    return 1

  return pad(n - 2) + pad(n - 3)

# DP
def pad_dp(n):
  dp = [0 for i in range(n + 1)]
  dp[0] = dp[1] = dp[2] = 1
  for i in range(3, n + 1):
    dp[i] = dp[i - 2] + dp[i - 3]

  return dp[-1]

# DP w/ constant space
def pad_dp_constant_space(n):
  if (n == 0 or n == 1 or n == 2):
    return 1

  first = second = third = 1
  for i in range(3, n + 1):
    fourth = first + second
    first = second
    second = third
    third = fourth

  return fourth