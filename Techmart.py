import requests
from bs4 import BeautifulSoup
import csv

# Премахване на SSL certificate verification
requests.packages.urllib3.disable_warnings()

# URL на Техмарт за Смартфони
url = 'https://techmart.bg/Смартфони/limit/48'

# request към уебсайта
response = requests.get(url, verify=False)

# Parsване на HTML съдържанието, като използваме BeautifulSoup
soup = BeautifulSoup(response.content, 'html.parser')

# Извличане на имената, цените и линковете
product_titles = soup.find_all('a', itemprop='url')
product_prices = soup.find_all('span', itemprop='price')
product_urls = soup.find_all('a', itemprop='url')

# Лист от речници, където всеки речник е отделен смартфон
products = []
for title, price, url in zip(product_titles, product_prices, product_urls):
    product = {
        'Име': title.text.strip() if title.text.strip() else url['href'].split('/')[3],
        # ако е празно взмима името от линка
        'Цена': price.text.strip(),
        'Url': url['href']
    }

    # request към product URL
    response = requests.get(product['Url'], verify=False)
    product_soup = BeautifulSoup(response.content, 'html.parser')

    # Взимане на линк към снимките
    image_tag = product_soup.find('img', itemprop='image')
    product['Image_url'] = image_tag['src'] if image_tag else ''

    # От веки отделен линк взимаме информацията за характеристиките
    characteristics_section = product_soup.find('article', id='characteristics')
    characteristics = characteristics_section.find_all('div', class_='nmclRow')
    for characteristic in characteristics:
        key, value = characteristic.text.split(':')
        product[key.strip()] = value.strip()

    products.append(product)

# Записване в csv файл
with open('products.csv', mode='w', encoding='utf-8', newline='') as file:
    fieldnames = ['Име', 'Цена', 'Url', 'Image_url', 'Размер на дисплея (inch)', 'Технология на дисплея',
                  'Вградена памет (GB)',
                  'RAM памет (GB)', 'Слот за карта памет', 'Резолюция', 'Операционна система',
                  'Опресняване на дисплея (Hz)', 'Модел процесор', 'Задна камера (MP)', 'Предна камера (MP)',
                  'Батерия (mAh)', 'DUAL SIM', 'Размер на дисплея (см)', 'Тегло (гр)', 'Марка', 'Модел', 'Гаранция',
                  'Тип процесор', 'Видео запис', 'Баркод', '4G', '5G', 'Fast Charging', 'Безжично зареждане']
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()
    # ако е празна характеристиката, записваме не е упоменато
    for product in products:
        for key in fieldnames[4:]:
            if key not in product:
                product[key] = 'не е упоменато'
        writer.writerow(product)

