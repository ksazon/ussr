from math import cos, asin, sqrt
from company import models

def distance(lat1, lon1, lat2, lon2):
    p = 0.017453292519943295
    a = 0.5 - cos((lat2-lat1)*p)/2 + cos(lat1*p)*cos(lat2*p) * (1-cos((lon2-lon1)*p)) / 2
    return 12742 * asin(sqrt(a))

def getClosest(u_lat, u_lon):
    closestBranch = ''
    closestDistance = -1
    for branch in CompanyBranch.objects:
        tmpDistance = distance(branch.latitude, branch.longitude, u_lat, u_lon)

        if closestDistance == -1 or closestDistance > tmpDistance:
            closestDistance = tmpDistance
            closestBranch = branch

    return closestBranch
