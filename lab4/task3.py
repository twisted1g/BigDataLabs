import csv
import random
import re
import time
from urllib.parse import urljoin

import requests
from bs4 import BeautifulSoup

# Task 3: Collect apartment listings (200+ records) from a real estate site
# Source URL from the assignment example (Cian, Krasnodar, sale flats).

BASE_URL = "https://krasnodar.cian.ru"
SEARCH_URL = (
    "https://krasnodar.cian.ru/cat.php"
    "?deal_type=sale&engine_version=2&offer_type=flat&p={page}&region=4820"
)

OUTPUT_CSV = "cian_apartments_krd.csv"
MAX_RECORDS = 220
MAX_PAGES = 30

USER_AGENTS = [
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
]


def clean_text(value: str) -> str:
    if not value:
        return ""
    value = re.sub(r"\s+", " ", value).strip()
    return value


def pick_first(*values):
    for value in values:
        if value:
            return value
    return ""


def extract_card_link(card) -> str:
    link = ""
    a = card.select_one('a[data-mark="OfferTitle"]')
    if a and a.get("href"):
        link = a.get("href")
    if not link:
        a = card.select_one("a")
        if a and a.get("href"):
            link = a.get("href")
    if link:
        return urljoin(BASE_URL, link)
    return ""


def extract_title(card) -> str:
    title = ""
    el = card.select_one('a[data-mark="OfferTitle"]')
    if el:
        title = el.get_text(strip=True)
    if not title:
        el = card.select_one("h3")
        if el:
            title = el.get_text(strip=True)
    return clean_text(title)


def extract_price(card) -> str:
    price = ""
    el = card.select_one('[data-mark="MainPrice"]')
    if el:
        price = el.get_text(strip=True)
    if not price:
        el = card.select_one("span[data-mark]")
        if el and "₽" in el.get_text():
            price = el.get_text(strip=True)
    return clean_text(price)


def extract_address(card) -> str:
    address = ""
    el = card.select_one('[data-name="Geo"]')
    if el:
        address = el.get_text(" ", strip=True)
    if not address:
        el = card.select_one('[data-mark="OfferAddress"]')
        if el:
            address = el.get_text(" ", strip=True)
    return clean_text(address)


def extract_characteristics(card) -> str:
    chars = ""
    el = card.select_one('[data-name="OfferSummary"]')
    if el:
        chars = el.get_text(" ", strip=True)
    if not chars:
        # Fallback: try to collect short summary lines
        lines = []
        for li in card.select("ul li"):
            text = clean_text(li.get_text(" ", strip=True))
            if text:
                lines.append(text)
        chars = "; ".join(lines[:5])
    return clean_text(chars)


def extract_photo(card) -> str:
    img = card.select_one("img")
    if img:
        return pick_first(img.get("src"), img.get("data-src"), img.get("data-original"))
    return ""


def parse_cards(html: str):
    soup = BeautifulSoup(html, "html.parser")

    # Several selectors to be resilient to minor layout changes
    selectors = [
        'div[data-name="CardComponent"]',
        'article[data-name="CardComponent"]',
        'div[data-name="ListingItem"]',
        'div._93444fe79c--card--ibP42',
    ]

    cards = []
    for sel in selectors:
        cards = soup.select(sel)
        if cards:
            break

    results = []
    for card in cards:
        title = extract_title(card)
        link = extract_card_link(card)
        price = extract_price(card)
        address = extract_address(card)
        characteristics = extract_characteristics(card)
        photo = extract_photo(card)

        if not title and not link:
            continue

        results.append(
            {
                "title": title,
                "address": address,
                "price": price,
                "characteristics": characteristics,
                "photo_url": photo,
                "link": link,
            }
        )

    return results


def fetch_page(page: int) -> str:
    url = SEARCH_URL.format(page=page)
    headers = {"User-Agent": random.choice(USER_AGENTS)}
    response = requests.get(url, headers=headers, timeout=30)
    if response.status_code != 200:
        raise RuntimeError(f"HTTP {response.status_code} for {url}")
    return response.text


def main():
    apartments = []
    seen_links = set()

    for page in range(1, MAX_PAGES + 1):
        print(f"Загружаем страницу {page}...")
        try:
            html = fetch_page(page)
        except Exception as exc:
            print(f"Ошибка при запросе страницы {page}: {exc}")
            break

        page_items = parse_cards(html)
        if not page_items:
            print("Объявления не найдены, остановка.")
            break

        new_items = 0
        for item in page_items:
            link = item.get("link") or ""
            if link in seen_links:
                continue
            seen_links.add(link)
            apartments.append(item)
            new_items += 1
            if len(apartments) >= MAX_RECORDS:
                break

        print(f"Найдено объявлений на странице: {len(page_items)}, новых: {new_items}")

        if len(apartments) >= MAX_RECORDS:
            break

        time.sleep(random.uniform(2.5, 6.5))

    print(f"Всего собрано объявлений: {len(apartments)}")

    with open(OUTPUT_CSV, "w", newline="", encoding="utf-8") as file:
        writer = csv.DictWriter(
            file,
            fieldnames=["title", "address", "price", "characteristics", "photo_url", "link"],
        )
        writer.writeheader()
        writer.writerows(apartments)

    print(f"Данные записаны в {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
