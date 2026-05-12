from scipy.spatial import Voronoi
import random as ra
import turtle as tu
import math as mt
import time as ti
from PIL import Image
import os
import threading as th
#file_path = os.path.abspath("musique.mp3")
#playsound(file_path)
tu.tracer(False)
WIDTH, HEIGHT = 500, 500

screen = tu.Screen()
screen.setup(WIDTH + 4, HEIGHT + 8)

tu.hideturtle()


def distance(pt1,pt2):
    return mt.sqrt((pt1[0]-pt2[0])**2+(pt1[1]-pt2[1])**2)
def rgb_to_hex(rgb):
    return "#{:02X}{:02X}{:02X}".format(*rgb)
def unsynced_rotation(points, amp = 0.01):
    points.sort(key=lambda pt: distance(pt, (0, 0)))
    for i in range(len(points)):
        x, y = points[i]
        if i%2 :
            change = amp
        else :
            change = -amp
        points[i] = (x * mt.cos(change)-y * mt.sin(change), x*mt.sin(change)+y*mt.cos(change))
    return points
def faux_deplacement(points, ampx = 10, ampy=0):
    for i in range(len(points)):
        x, y = points[i]
        points[i] = ((x + 260) % 500 - 250, y)
    return points


def fancy(txt,amp = 3, font = ('Arial', 8, 'normal')):
    txt = str(txt)
    Xpos, Ypos = tu.xcor(), tu.ycor()
    for i, lettre in enumerate(txt):
        tu.goto(tu.xcor(), Ypos + amp*mt.cos((i+ti.time()*3)))

        tu.write(lettre,move=True, font = font)
    tu.goto(Xpos, Ypos)

def draw_voronoi(voronoi):
    vor_regions = voronoi.regions
    vor_vertices = voronoi.vertices
    for i, poly in enumerate(vor_regions):
        if -1 not in poly and poly != []:
            sommex, sommey = 0, 0
            for pt in poly:
                sommex += vor_vertices[pt][0]
                sommey -= vor_vertices[pt][1]
            point = [sommex / len(poly), sommey / len(poly)]

            tu.pu()
            x, y = vor_vertices[poly[0]]
            tu.goto(x, y)
            tu.pd()
            for point in range(-1, len(poly)):
                x, y = vor_vertices[poly[point]]
                tu.goto(x, y)
    tu.pu()
def decor():
    global points_decor
    # Ajout

    # Woobling
    # points = [[point[0]+1*ra.randint(-1,1), point[1]+1*ra.randint(-1,1)] for point in points]
    # Rotation unsynced
    points_decor = unsynced_rotation(points_decor,amp=0.005)

    vor = Voronoi(points_decor + debug)
    tu.color('gray')
    draw_voronoi(vor)
def x_change(amp):
    tu.setx(tu.xcor()+amp)
def y_change(amp):
    tu.sety(tu.ycor()+amp)
def main():
    global game_mode, begin_time, challenge
    tu.pu()
    old_gm = game_mode
    begin_time = ti.time()
    challenge = None
    attent = False
    mesure = ti.time()
    while True:
        if old_gm != game_mode:
            begin_time = ti.time()
            mesure = 0
            old_gm = game_mode
        else:
            mesure = ti.time() - begin_time
        tu.clear()
        if game_mode == "menu":

            decor()
            # menu text
            tu.color('black')
            tu.goto(-185, 100)
            fancy("Voronoï's minigames ", font = ('Arial', 30, 'bold'))
            x_change(-10)
            y_change(-25)
            fancy("Meilleur score : 10", font = ('Arial', 10, 'normal'), amp = 1.5)
            tu.goto(-135,-200)
            fancy("Appuyez sur clic gauche pour jouer", font=('Arial', 12, 'italic'), amp=1)

        elif game_mode == "trans":
            decor()
            tu.color('black')
            # animation des chiffres 1, 2, 3

            if mesure > 3 and mesure < 4:
                # Choisi un mode au hasard non identique a l'ancien
                if not attent:
                    challenge = ra.choice([i for i in range(5) if i != challenge])
                    attent = True
                if challenge == 0:
                    tu.goto(-70, 0)
                    fancy("Quesaquo ?", font=('Arial', 25, 'bold'), amp = 3)
                elif challenge == 1:
                    tu.goto(-160, 0)
                    fancy("Capturez la zone !", font=('Arial', 25, 'bold'), amp = 3)
                elif challenge == 2:
                    tu.goto(-170, 0)
                    fancy("Débouchez le chemin !", font=('Arial', 25, 'bold'), amp = 3)
                elif challenge == 3:
                    tu.goto(-150, 0)
                    fancy("Eliminez le rouge !", font=('Arial', 25, 'bold'), amp = 3)
                else:
                    tu.goto(-220, 0)
                    fancy("Pavez l'espace selon le dessin !", font=('Arial', 25, 'bold'), amp = 3)
            elif mesure > 4:
                game_mode = "minigame"
                # Réinitialise les variables d'outils
                attent = False
            else:
                x_displ = (mesure % 1) * 500 - 250
                tu.goto((0.05*x_displ)**3, 0)
                if mesure < 1:
                    fancy("1", font=('Arial', 45, 'bold'))
                elif mesure < 2:
                    fancy("2", font=('Arial', 45, 'bold'))
                elif mesure < 3:
                    fancy("3", font=('Arial', 45, 'bold'))
        elif game_mode == "minigame":
            if challenge != -1:
                print(None)
                quesaquo()
            if challenge == 0:
                quesaquo()
            elif challenge == 1:
                capture()
            elif challenge == 2:
                debouche()
            elif challenge == 3:
                anti_rouge()
            elif challenge == 4:
                ressemblance()

        print(mesure)
        tu.update()
def capture():
    return
def debouche():
    return
def anti_rouge():
    return
def ressemblance():
    return
def quesaquo():
    # Importer image
    selection = ["nhat", "koala", "canard", "chocolatine", "rubik", "chat"]
    reponse = ra.choice(selection)
    image_path = f"{reponse}.jpg"
    image = Image.open(image_path)
    image = image.convert("RGB")
    # avoir dimension
    width, height = image.size

    # Access pixel values
    pixels = image.load()  # Load pixel data


    points = [[ra.uniform(-250, 250), ra.uniform(-250, 250)] for i in range(100)]
    vor = Voronoi(points + debug)

    #Get voronoi vertices
    # Example: Access the pixel at (x, y)
    vor_vertices = vor.vertices
    #Get voronoi regions

    vor_regions = vor.regions

    while game_mode == "minigame":
        tu.clear()
        # Ajout
        for i in range(10):
            points.append([ra.uniform(-250, 250),ra.uniform(-250, 250)])
        # Woobling
        #points = [[point[0]+1*ra.randint(-1,1), point[1]+1*ra.randint(-1,1)] for point in points]
        # Rotation unsynced
        #points = unsynced_rotation(points)
        #points = faux_deplacement(points)

        vor = Voronoi(points + debug)
        vor_regions = vor.regions
        vor_vertices = vor.vertices
        for i, poly in enumerate(vor_regions) :
            if -1 not in poly and poly != []:
                sommex, sommey = 0, 0
                for pt in poly:
                    sommex += vor_vertices[pt][0]
                    sommey -= vor_vertices[pt][1]
                point = [sommex / len(poly), sommey / len(poly)]
                if -250 >= point[0]:
                    point[0] = -249
                elif 250 <= point[0]:
                    point[0] = 249
                if -250 >= point[1]:
                    point[1] = -249
                elif 250 <= point[1]:
                    point[1] = 249
                x, y = int(int((point[0] + 250) % 500) * width / 500), int(int((point[1] + 250) % 500) * height / 500)

                tu.fillcolor(rgb_to_hex(pixels[x, y]))

                tu.pu()
                x, y = vor_vertices[poly[0]]
                tu.goto(x, y)
                tu.begin_fill()
                for point in range(-1, len(poly)):
                    x, y = vor_vertices[poly[point]]
                    tu.goto(x, y)
                tu.end_fill()
        tu.pu()
        tu.goto(-165, -200)
        tu.color("black")
        fancy("chat", font=('Arial', 18, 'bold'))
        tu.color("white")
        fancy("chat", font=('Arial', 15, 'bold'))

        tu.goto(-25, -200)
        tu.color("black")
        fancy("chat", font=('Arial', 18, 'bold'))
        tu.color("white")
        fancy("chat", font=('Arial', 15, 'bold'))

        tu.goto(165, -200)
        tu.color("black")
        fancy("chat", font=('Arial', 18, 'bold'))
        tu.color("white")
        fancy("chat", font=('Arial', 15, 'bold'))

        tu.update()

points_decor = [[ra.uniform(-250, 250), ra.uniform(-250, 250)] for i in range(100)]
debug = [[-10000, -10000], [10000, -10000], [10000, 10000], [-10000, 10000]]
begin_time = ti.time()
challenge = None
tu.listen()
game_mode = "menu"
def get_mouse_click_coor(x,y):
    global game_mode, challenge, selected
    if game_mode == "menu":
        game_mode = "trans"
    if game_mode == "minigame":
        if challenge != -1:
            if x < -83:
                selected = 0
            elif x < 83:
                selected = 1
            else:
                selected = 2
            print(selected)
    print(game_mode, challenge)
tu.onscreenclick(get_mouse_click_coor)
main()
tu.mainloop()


