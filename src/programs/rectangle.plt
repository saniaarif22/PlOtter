#In future it'll be from library
fn rect(point a, num h, num w):
    num i
    num x
    num y
    point b
    /* Make a rectanle by drawing multiple lines */
    for i=0;i<w;i=i+1:
        x = a[0]
        y = a[1]
        line( (x+i , y), (x+i, y+h))
    end
end

point a
a = (100,100)

#Rectangles with increasing width, nice pattern too
rect(a, 200, 10)
rect((100+15,100), 200, 50)
rect((100+70,100), 200, 100)
rect((100+175,100), 200, 200)
rect((100+380,100), 200, 500)
rect((100+885,100), 200, 500)
