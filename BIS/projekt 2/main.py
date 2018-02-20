#!/usr/local/bin/python
import sys
import re
import os
import json
import datetime
import email
import warnings
from email import message_from_file
from collections import Counter
from pprint import pprint
import codecs
import os.path

# Functions pulls out all plain-text / html
def get_text(message):
    Html = ""
    Text = ""
    i = 0
    if not message.is_multipart():
        # Not multipart message
        cp = message.get_content_type()
        if cp == "text/plain":
            Text += message.get_payload(decode=True)
        elif cp == "text/html":
            Html += message.get_payload(decode=True)
        return Text, Html

    while True:
        try:
            part = message.get_payload(i)
        except:
            break

        tmp_text, tmp_html = get_text(part)
        Text += tmp_text
        Html += tmp_html
        i += 1
    return Text, Html


# Function returns plain text from a eml file
def extract(msgfile):
    message = message_from_file(msgfile)
    Text, Html = get_text(message)
    ret = {"text": Text.strip(), "html": Html.strip()}
    return ret


# Function for creating single json file of an email dataset
# Counts occurences of all unique words in a dataset
# Saves into a json file
# Used only in Train phase, not in Classify phase
def train(emlType):
    # Declare total vars Counter and int
    total_dict = Counter()
    total_spam = 0
    # Iterate over spam .eml
    for filenames in os.listdir(emlType + '/'):
        with open(os.path.join(emlType + '/', filenames)) as myfile:
            # Increment total spam counter
            total_spam += 1
            extracted = extract(myfile)
            if len(extracted['text']):
                tmpdict = Counter(re.sub('<[^<]+?>', '', extracted['text']).lower().split())
            elif len(extracted['html']):
                tmpdict = Counter(re.sub('<[^<]+?>', '', extracted['html']).lower().split())
            else:
                pass

            tmpdict = { x:1 for x in tmpdict}
            # Add Counters
            total_dict = total_dict + Counter(tmpdict)        
    # Dump dictionary json into file
    with open('trained-'+ emlType +'.json', 'w') as file:
        file.write(json.dumps(dict(total_dict), encoding='latin1'))
    # Write total spam count into file
    with open('total-'+ emlType +'.txt','w') as f:
        f.write(str(total_spam))

# Function to calculate probability that specific word belongs to spam/ham
# Returns float <0, 1> if word occurs in the train set
# Returns false if word does not occur in train set
def word_probability(emlType, word, spamset, hamset, totalSpamCount, totalHamCount):
    # Calculate possibility of occurence in Spam email
    if word in spamset:
        # Account probability
        occurences = spamset[word]        
        spam_probability = (occurences * 1.0) / totalSpamCount
    else:
        spam_probability = False

    # Calculate possibility of occurence in Ham email
    if word in hamset:
        # Account probability
        occurences = hamset[word]        
        ham_probability = (occurences * 1.0) / totalHamCount
    else:
        ham_probability = False

    # Calculate spaminess / haminess
    spaminess = False
    haminess = False
    if emlType == 'spams':
        if ham_probability and spam_probability:
            spaminess = (spam_probability * 1.0) / (spam_probability + ham_probability)
            return spaminess
    else:
    	if ham_probability and spam_probability:
            haminess = (ham_probability * 1.0) / (spam_probability + ham_probability)
            return haminess

    

def classify(emlFile, stopwords):
    with codecs.open(emlFile, "rb",encoding='utf-8', errors='ignore') as myfile:
        extracted = extract(myfile)

    if len(extracted['text']):
        wordList = re.sub('<[^<]+?>', '', extracted['text']).lower().split()
    elif len(extracted['html']):
        wordList = re.sub('<[^<]+?>', '', extracted['html']).lower().split()
    else:
    	wordList = []
    wordList = [word for word in wordList if word not in stopwords and len(word) > 2]

    # Trigger words check
    triggers = open('triggers.json')
    triggerset = json.load(triggers)
    for word in wordList:
    	if word in triggerset:
    		print(emlFile + ' - SPAM - contains word ' + word)
    		return

    # Load trained sets
    fSpamset = open('trained-spams.json')
    fHamset = open('trained-emails.json')
    spamset = json.load(fSpamset)
    hamset = json.load(fHamset)
    fHamset.close()
    fSpamset.close()

    # Get total spam count 
    with open('total-spams.txt','r+') as f:
        totalSpamCount = int(f.read())
        f.close()

    # Get total ham count
    with open('total-emails.txt','r+') as f:
        totalHamCount = int(f.read())
        f.close()

    # Calculate probability of Spam
    total_spam_probability = 1
    for word in wordList:
        probability = word_probability('spams', word, spamset, hamset, totalSpamCount, totalHamCount)
        if probability:
            total_spam_probability *= probability

    # Calculate probability of Ham
    total_ham_probability = 1
    for word in wordList:
        ham_probability = word_probability('emails', word, spamset, hamset, totalSpamCount, totalHamCount)
        if ham_probability:
            total_ham_probability *= ham_probability

    if total_ham_probability > total_spam_probability:
        print(emlFile + ' - OK' + ' - bayes classifier')
        return
    else:
    	print(emlFile + ' - SPAM' + ' - bayes classifier')
    	return

# Main
def main(argv):
    warnings.filterwarnings("ignore")
    
    #train('emails')
    
    # Open stopwords file and load json
    fStopwords = open('stopwords-en.json')
    stopwords = json.load(fStopwords)

    #train('emails')
    for filename in sys.argv[1:]: #os.listdir('emaily/'):
        if os.path.isfile(filename):
            classify(filename, stopwords)
        else:
            print filename + ' - FAIL - Not a file'

    # Close stopwords
    fStopwords.close()


if __name__ == "__main__":
    main(sys.argv)