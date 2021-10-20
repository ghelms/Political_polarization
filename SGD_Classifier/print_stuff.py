import sys
import os

filename=sys.argv[0]
path=os.path.dirname(os.path.abspath(__file__))

print("script: "+path+"/"+filename)


