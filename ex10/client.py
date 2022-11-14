# Stamatelopoulos Nikolaos
# 03116138

import requests
from sys import argv
from lxml import html

morse = {
    "·–": "a",
    "–···": "b",
    "–·–·": "c",
    "–··": "d",
    "·": "e",
    "··–·": "f",
    "––·": "g",
    "····": "h",
    "··": "i",
    "·–––": "j",
    "–·–": "k",
    "·–··": "l",
    "––": "m",
    "–·": "n",
    "–––": "o",
    "·––·": "p",
    "––·–": "q",
    "·–·": "r",
    "···": "s",
    "–": "t",
    "··–": "u",
    "···–": "v",
    "·––": "w",
    "–··–": "x",
    "–·––": "y",
    "––··": "z",
    "·––––": "1",
    "··–––": "2",
    "···––": "3",
    "····–": "4",
    "·····": "5",
    "–····": "6",
    "––···": "7",
    "–––··": "8",
    "––––·": "9",
    "–––––": "0",
    "": " " # just a whitespace between words
    }


if(len(argv) < 2):
    print("Please provide only target url as argument.")

url = argv[1]
session = requests.Session()

while(True):
    # get page and parse to find the question
    page = session.get(url)
    if(page.status_code != 200):
        print("Please make sure that the url is typed correctly and the page is up.")
        break
    tree = html.fromstring(page.content)
    question = tree.xpath('//code/text()')[0]
    # split phrase by whitespaces and decode
    tokens = question.split(" ")
    answer = ""
    for token in tokens:
        answer += morse[token]
    # submit answer and get result
    result = session.post(url, data={'submit': "submit", 'answer': answer})
    if(page.status_code != 200):
        print("Please make sure that the page accepts post requests.")
        break
    # just check response content to see that everything is fine
    tree = html.fromstring(result.content)
    correct = tree.xpath('//p[@class="right"]/text()')
    wrong = tree.xpath('//p[@class="wrong"]/text()')
    # check if we reached the end through the play again value in the button
    end = tree.xpath('//form/input[@id="again" and @name="again" and @value="Play again!"]')
    if len(correct) > 0:
        print(answer)
    elif len(wrong) > 0:
        print("My guess was wrong: \""+answer+"\"")
    else:
        print("Something went wrong!")
    if len(end) > 0:
        #print("I beat the game!!!")
        break
    session.post(url, data={'continue': "continue"})

session.close()
