include plots
#Rectabgle with black border
fn rectWB(point p, num w, num h, string c):
    rectFill(p,w,h,c)
    rect(p,w,h)
end



rectWB((0,550),50, 300,"grey")

rect((65,135),465, 170)
rectWB((85,120),480, 130,"grey")
rectWB((90,120),480, 120,"white")

rectWB((100,100),500, 100,"grey")

#Centre row
rectWB((125,50),550, 50,"white")
rectWB((145,0),600, 10, "white")

#Pillars
rectWB((50,150),450, 70, "white")
rectWB((50,200),400, 70, "white")
rectWB((180,150),450, 70, "white")
rectWB((180,200),400, 70, "white")











