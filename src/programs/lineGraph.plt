#include plots
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

fn drawAxes(num ox, num oy, num maxLength, num maxHeight):
    
    #Draw axes
    rect((ox,0), maxHeight, 3)
    rect((0,maxHeight-oy),3,maxLength)
end

fn lineGraph(list num a):
    num maxLength
    num maxHeight
    num maxDataHt
    num maxDataLn
    num gap
    num scaleFactor
    num padHz
    num padVt
    point p1
    point p2
    num i
    
    
    #Setting the dimensions of the graph
    maxLength = 640
    maxHeight = 480
    
    #Max ht
    maxDataHt = a[0]
    for i=0;i<a.length();i=i+1:
        if a[i] > maxDataHt:
            maxDataHt = a[i]
        end
    end
    
    #max length
    maxDataLn = a.length()
    
    #padding
    padHz = 10
    padVt = 10
    
    #Line graph settings 10% of the graph
    gap         = (maxLength - padHz) / maxDataLn
    scaleFactor = (maxHeight - padVt) / maxDataHt
    
    #Draw the bars, scaled and with the gap
    num x
    num j
    num y1
    num y2
    x = padHz
    
    if a.length() > 0:
        
        for i=1;i<a.length();i=i+1:
            #y1 = a[j] * scaleFactor
            #y2 = a[i] * scaleFactor
            line( (x , maxHeight - a[ i - 1 ] * scaleFactor) , ( x + gap , maxHeight- a[ i ]*scaleFactor) )
            rect ( (x - 2.5, maxHeight - a[ i - 1 ] * scaleFactor - 2.5) , 5 , 5)
            rect ( (x - 2.5, maxHeight - a[ i - 1 ] * scaleFactor - 2.5) , 5 , 5)
            x = x + gap
            
        end
    end
end

list num a
a = [50,53, 55, 53, 50, 45, 25, 10, 5, 3, 0, 2, 4, 10, 25, 45,60, 75, 85, 90,95, 98, 99, 100]
lineGraph(a)
drawAxes( 10,240,640,480)


