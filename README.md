# singer: a twitter thread reconstructor

Singer is a basic library dedicated to reading the data dumps provided by
Twitter to extract _threads_ you posted. Hence the name: it's like a sewing
machine.

It uses the Streaming library (and related libraries) to parse the often huge
file containing your tweets provided by twitter without using too much RAM.

Current status:

- It won't handle broken threads (i.e., if your thread becomes actually a tree
  and not a straight line because you answered the same tweet several time).
- Main logic should be rewritten using a fold

Do note that for some mysterious reason, Tweeter do not provide the "tweet.js"
file containing all your tweets as a JSON file but as JS file. A small amount of
modification on your file will be needed so that it can be parsed properly.
