def naif(t,m):
    for i in range(len(t)-len(m)):
        b=True
        for j in range(len(m)):
            print(t[i+j])
            print(m[j])
            if t[i+j] != m[j]:
                b = False
        if b : 
            return True
    return False


t = ['c','o','u','c','o','u','s','e','q']
m = ['u','c']
naif(t,m)