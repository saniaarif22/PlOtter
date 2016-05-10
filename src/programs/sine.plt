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

num i
num ht
num flag

#Initial
flag = 1

# Trying to print a sine wave with rectange function 
for i=0;i<1000;i=i+1:
    rect((100+5*i,100+ht),200,4)
    
    ht = ht + flag*2
    #Sine switch
    if ht==100 or ht==0:
        flag = flag* -1
    end
end
