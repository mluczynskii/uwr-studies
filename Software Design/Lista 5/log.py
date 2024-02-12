# script for generating fake server logs 
from random import choice, randint
import datetime

users = ["192.168.0.1", "114.151.57.8", "37.152.252.211", "47.23.26.144", "31.173.138.223", "40.127.222.41"]
codes = [200, 404, 202, 401]
root = "/funnycats/"
resources = ["bingus.jpg", "pixel.jpg", "uni.jpg", "jinx.jpg"]

def timestamp_gen(start, count):
    curr = start 
    for _ in range(count):
        curr = curr + datetime.timedelta(minutes=randint(0, 60))
        yield curr 

N = 2000
start = datetime.datetime.now()

with open("server.log", 'w') as logs:
    for timestamp in timestamp_gen(start, N):
        date = timestamp.strftime("%H:%M:%S")
        logs.write(f'{date} {choice(users)} GET {root}{choice(resources)} {choice(codes)} \n') 

    


