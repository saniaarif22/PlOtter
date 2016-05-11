fn rect(point a, num h, num w):
    line(a,(a[0]+w,a[1]))
    line(a,(a[0],a[1]+h))
    line((a[0]+w,a[1]),(a[0]+w,a[1]+h))
    line((a[0],a[1]+h),(a[0]+w,a[1]+h))
end

rect ( (100,100), 50, 20)
