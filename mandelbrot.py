left_edge = -520
right_edge = 400
top_edge = 400
bottom_edge = -400
x_step = 1
y_step = 3
max_iter = 300

for y0 in range(bottom_edge, top_edge, y_step):
    for x0 in range(left_edge, right_edge, x_step):
        x = 0
        y = 0
        char = " "
        i = 0
        while i < max_iter:
            x_x = x**2 / 200
            y_y = y**2 / 200
            if x_x + y_y > 800:
                char = str(i)
                if i > 9:
                    char = "@"
                i = max_iter
            y = x * y / 100 + y0
            x = x_x - y_y + x0
            i += 1
        print(char, end="")
    print(" ")
