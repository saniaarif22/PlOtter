fn rect(point a, num h, num w, string color):
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

rectFill ( (100,100), 50, 20,"blue")



