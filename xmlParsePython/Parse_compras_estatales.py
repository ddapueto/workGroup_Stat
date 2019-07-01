import json
import urllib.request
import pandas as pd
from datetime import date, timedelta
import xmltodict
from tqdm import tqdm




# funcion para lista de dias en semanas , motivo el get web maximo 10 dias

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

    return dias # list days to work in format datetime

def getinfoweb(tuple_days):

    initialUrl = "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/generarReporte?tipo_publicacion=a&tipo_compra=&rango-fecha="

    #generate from and to url

    year_from = tuple_days[0].strftime('%Y')
    month_from = tuple_days[0].strftime('%m')
    month_from_dist = tuple_days[0].strftime("%-m")
    day_from = tuple_days[0].strftime('%d')

    year_to = tuple_days[1].strftime('%Y')
    month_to = tuple_days[1].strftime('%m')
    month_to_dist = tuple_days[1].strftime("%-m")
    day_to = tuple_days[1].strftime('%d')

    new_url = initialUrl + day_from+"%2F"+month_from+"%2F"+year_from+"+-+"+day_to+"%2F"+month_to+"%2F"+year_to+"&dia_inicial="+day_from+"&mes_inicial="+month_from_dist+"&anio_inicial="+year_from+"&hora_inicial=0&dia_final="+day_to+"&mes_final="+month_to_dist+"&anio_final="+year_to+"&hora_final=23"

    webURL = urllib.request.urlopen(new_url)
    data = webURL.read()
    encoding = webURL.info().get_content_charset('utf-8')
    st = data.decode(encoding)

    return st

if __name__ == "__main__":

    start_date = date(2018, 2, 15)
    end_date = date(2018, 2, 16)

    listadayswork = dayslist(start_date, end_date)



    for daysweek in tqdm(listadayswork):
        print(daysweek)

        compras = pd.DataFrame()
        adju = pd.DataFrame()
        ofera = pd.DataFrame()

        infoweb = getinfoweb(daysweek)

        print("finish import data")
        doc = xmltodict.parse(infoweb)
        for key, value in doc.items():
            for key1, value1 in value.items():
                for key2, value2 in value1.items():
                    # print(key2, type(value2))
                    if key2 == 'compra':
                        for a in value2:
                            json_compras = {}
                            for key3, value3 in a.items():
                                if key3 == "adjudicaciones":
                                    json_adju = {}
                                    for key4, value4 in value3.items():
                                        id_com = json_compras['@id_compra']
                                        json_adju['@id_compra'] = id_com
                                        i = 1
                                        if type(value4) is list:
                                            for c in value4:
                                                json_adju['key_id'] = i
                                                for key5, value5 in c.items():
                                                    json_adju[key5] = value5
                                                adju = adju.append(json_adju, ignore_index=True)
                                                i = i + 1
                                        else:
                                            json_adju['key_id'] = i
                                            for key5, value5 in value4.items():
                                                json_adju[key5] = value5
                                            adju = adju.append(json_adju, ignore_index=True)

                                if key3 == "oferentes":
                                    json_oferante={}
                                    for key4o, value4o in value3.items():
                                        id_com = json_compras['@id_compra']
                                        json_oferante['@id_compra'] = id_com
                                        i = 1
                                        if type(value4o) is list:
                                            for value4oItem in value4o:
                                                json_oferante['key_id'] = i
                                                for key5o, value5o in value4oItem.items():
                                                    json_oferante[key5o] = value5o
                                                ofera = ofera.append(json_oferante, ignore_index=True)
                                                i = i+1
                                        else:
                                            json_oferante['key_id'] = i
                                            for key5o, value5o in value4o.items():
                                                json_oferante[key5o] = value5o
                                            ofera = ofera.append(json_oferante, ignore_index=True)
                                else:
                                    json_compras[key3] = value3
                            compras = compras.append(json_compras, ignore_index=True)

        print(compras.shape)
        print(adju.shape)
        print(ofera.shape)

        fecha = daysweek[0].strftime('%Y_%m_%d')
        com = 'comprasEstatales'+fecha+'.csv'
        adj = 'comprasEstatalesAdjudicadas' + fecha + '.csv'
        ofe = 'comprasEstatalesOferantes' + fecha + '.csv'
        compras.to_csv(com)
        adju.to_csv(adj)
        ofera.to_csv(ofe)

