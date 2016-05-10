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
