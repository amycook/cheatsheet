print "Hello World!"

print "tomatoes cost $", 3+4, "/kg"

print 7/4
print 7.0 / 4.0

cars = 100
why = 25
print cars/why


print "cars"
cars = 100
why = 25
print 100.0/25.0

my_name = "Ames"
weight = 70.0
new = 80
print "Let's talk about %s." % my_name
print "I weigh %d" %weight
print "I weigh %d, %d" % (new, weight)
print "I weigh %r, %r" % (my_name, weight)



# More about strings - exercise 6
x = 'there are %d types of people' % 10
bina = 'binary'
do_not = "don't"
y = 'those who know %s and those who %s.' % (bina, do_not)
print x
print y
print 'i said: %s' %x
print 'i said: %r' %x

hilarious= False
joke_eval = "Isnt this joke funny? %r"
print joke_eval % hilarious

w = 'this is the left side of'
e = 'a joke'
print w , e
print w + e

# exercise 7
print "." * 10
print 6 * 10

end1 = "C"
end2 = "h"
end3 = 'e'
end4 = 'e'
print end1 + end2,
print end3 + end4

# exercise 9
print """
Why can.
I do this.
like this.

"""

# exercise 10
#\t is a tab
# inside """, don't have to escape out of ' or "
fat_cat = """
I'll do a list:
* Cat food 
\t* Fishies
\t* Catnip\n\t* Grass
"""

print fat_cat
