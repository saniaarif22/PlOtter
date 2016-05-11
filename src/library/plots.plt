/*
    Method :
        rect(point xy, num h, num w)
    
    Return :
        void
        
    Description :
        Prints a rectangle, from point x,y with height h and width w The function rectangle supported as libray.
    
    Future Todo :
        Send hash, to set the properties of the graph like axis, grid, title etc 
 */
fn rect(point a, num h, num w):
    line(a,(a[0]+w,a[1]))
    line(a,(a[0],a[1]+h))
    line((a[0]+w,a[1]),(a[0]+w,a[1]+h))
    line((a[0],a[1]+h),(a[0]+w,a[1]+h))
end

fn rectFill(point a, num h, num w, string color):
    num i
    point x
    point y
    string s
    
    /* Make a rectanle by drawing multiple lines */
    for i=0;i<w;i=i+1:
        x = (a[0]+i, a[1])
        y = (a[0]+i, a[1]+h)    
        line( x , y , color )
    end
end

/*
    Method :
        drawAxes(num xorigin, num yorigin, num maxWidth, num maxHeight)
    
    Return :
        void
        
    Description :
        Draws cross axis from the passed origin point. Takes max width and max Height to plot lines
    
    Future Todo :
        Send hash, to set the properties of the graph like axis, grid, title etc 
 */
fn drawAxes(num ox, num oy, num maxLength, num maxHeight):
    
    #Draw axes
    rect((ox,0), maxHeight, 3)
    rect((0,maxHeight-oy),3,maxLength)
end

/*
    Method :
        barGraph(<num list var>)
    
    Return :
        void
        
    Description :
        Prints a fitting barGraph for the given data. The function bargraph supported as libray. Users can write their own too. It takes the list of num and automatically fits it in the graph based on the data.
    The user just needs to call the function with the data
    
    Future Todo :
        Send hash, to set the properties of the graph like axis, grid, title etc 
 */
fn barGraph(list num a):
    num maxLength
    num maxHeight
    num maxDataHt
    num maxDataLn
    num gap
    num scaleFactor
    num padHz
    num padVt
    num barWidth
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
    
    #bar graph settings 10% of the graph
    gap         = 0.1 * (maxLength - padHz) / maxDataLn
    barWidth    = 0.9 * (maxLength - padHz) / maxDataLn
    scaleFactor = (maxHeight - padVt) / maxDataHt
    
    #Draw the bars, scaled and with the gap
    num x
    x = padHz
    for i=0;i<a.length();i=i+1:
        #Drawing the bar
        rect( (x,maxHeight- a[i]*scaleFactor) , a[i]*scaleFactor , barWidth )
        #line( (x,maxHeight- a[i]*scaleFactor) , (x,maxHeight) )
        #print x
        x = x + barWidth + gap
    end
    
    
end

/*
    Method :
        lineGraph(num xorigin, num yorigin, num maxWidth, num maxHeight)
    
    Return :
        void
        
    Description :
        Draws a line graph  based on the points give. To draw axes call it seperately
    
    Future Todo :
        Send hash, to set the properties of the graph like axis, grid, title etc 
*/
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
            line( (x , maxHeight - a[ i - 1 ] * scaleFactor) , ( x + gap , maxHeight- a[ i ]*scaleFactor) )
            
            #dots on the data points
            rect ( (x - 2.5, maxHeight - a[ i - 1 ] * scaleFactor - 2.5) , 5 , 5)
            rect ( (x - 2.5, maxHeight - a[ i - 1 ] * scaleFactor - 2.5) , 5 , 5)
            x = x + gap
            
        end
    end
end
