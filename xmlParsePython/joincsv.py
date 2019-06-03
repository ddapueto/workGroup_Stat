import pandas as pd
from datetime import date, timedelta



def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days / 7)):
        nweek = n * 7
        yield start_date + timedelta(nweek)

def dayslist(start_date, end_date):

    i = 0
    dias = []


    for single_date in daterange(start_date, end_date):

        if i == 0:
            a = (single_date, single_date + timedelta(7))
            i = i + 1
        else:
            a = (single_date + timedelta(1), single_date + timedelta(7))
            i = i + 1
        dias.append(a)

    return dias

if __name__ == "__main__":
    start_date = date(2018, 8, 7)
    end_date = date(2019, 6, 1)

    listadayswork = dayslist(start_date, end_date)

    dfcom = pd.read_csv('comprasEstatales2018_01_01.csv')
    dfadj = pd.read_csv('comprasEstatalesAdjudicadas2018_01_01.csv')
    dfofe = pd.read_csv('comprasEstatalesOferantes2018_01_01.csv')

    for daysweek in listadayswork:
        fecha = daysweek[0].strftime('%Y_%m_%d')
        com = 'comprasEstatales'+fecha+'.csv'
        adj = 'comprasEstatalesAdjudicadas' + fecha + '.csv'
        ofe = 'comprasEstatalesOferantes' + fecha + '.csv'

        df1 = pd.read_csv(com)
        df2 = pd.read_csv(adj)
        df3 = pd.read_csv(ofe)

        dfcom = dfcom.append(df1, sort=False)
        dfadj = dfadj.append(df2, sort=False)
        dfofe = dfofe.append(df3, sort=False)
        print("finish join {}".format(fecha))

    dfcom.to_csv('comprasEstatales.csv')
    dfadj.to_csv('comprasEstatalesAdjudicadas.csv')
    dfofe.to_csv('comprasEstatalesOferantes.csv')
    print("Finish All")