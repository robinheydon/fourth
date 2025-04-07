names_zig = open ("src/names.zig").read ()
lines = names_zig.split ("\n")
scores = {
    'a': 5,
    'b': 1,
    'c': 3,
    'd': 2,
    'e': 5,
    'f': 1,
    'g': 3,
    'h': 3,
    'i': 5,
    'j': 3,
    'k': 3,
    'l': 2,
    'm': 1,
    'n': 3,
    'o': 5,
    'p': 1,
    'q': 3,
    'r': 2,
    's': 3,
    't': 2,
    'u': 5,
    'v': 1,
    'w': 1,
    'x': 3,
    'y': 4,
    'z': 2,
};

inside = False
for line in lines:
    if line.startswith ("const last_names"):
        inside = True
    elif line.startswith ("};"):
        inside = False
    elif inside:
        line = line.replace ("\"", "")
        line = line.replace (",", "")
        line = line.replace (" ", "")
        line = line.lower ()
        last_score = 0
        upwards = True
        scored = ""
        for ch in line:
            score = scores[ch]
            if score == 5:
                print (f"{ch}", end = '')
                scored += f"^v{score}"
                upwards = False
            elif upwards and score >= last_score:
                print (f"{ch}", end = '')
                scored += f"^{score}"
            elif not upwards and score <= last_score:
                print (f"{ch}", end = '')
                scored += f"v{score}"
            else:
                print (f"-{ch}", end = '')
                scored += f"-^{score}"
                upwards = True
            last_score = score
        print (f" : {scored}")
