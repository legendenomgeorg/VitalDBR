#testing vital db load
import vitaldb
import pandas as pd


df = vitaldb.load_trk("afd182c102c5af625d3f217280b3766d453d9e3f", interval=10)
print([i for i in df])