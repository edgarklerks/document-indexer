#!env python 

from elasticsearch import Elasticsearch
import ctags
import commands as c
from ctags import CTags, TagEntry
import sys
import os

try:
    ctags = sys.argv[1] 
    projectdir = sys.argv[2]
except IndexError:
    print "Need a exurbitant ctags binary and a project dir"
    sys.exit(1)

stat, output = c.getstatusoutput("%s -f /tmp/ctags-blub -R %s" % (ctags, projectdir))
if stat != 0:
    print "Error when running ctags: %s" % output
    sys.exit(1)

try:
    tagfile = CTags("/tmp/ctags-blub")
except:
    print "Error when reading ctags file"
    sys.exit(1)

# collect all tags for a sorted tag

fileCollection = dict()
entry = TagEntry()
status = tagfile.first(entry)
fileCollection[entry['file']] = [(entry['name'], entry['kind'], entry['lineNumber'], entry['pattern'])]
while tagfile.next(entry):
    addEntry = [(entry['name'], entry['kind'], entry['lineNumber'], entry['pattern'])]
    try: 
        fileCollection[entry['file']] += addEntry
    except KeyError:
        fileCollection[entry['file']] = addEntry

es = Elasticsearch()
for file in fileCollection:
    f = open(file, "r")
    tags = [] 
    for tag in fileCollection[file]: 
        print tag
        tags += [{
            "tag": tag[0],
            "kind":tag[1],
            "sourceline":tag[2],
            "pattern":tag[3]
            }]

    es.create(index="documents", doc_type="doc", body={
         "path":file,
         "content":f.read(),
         "type": "sourcefile",
         "path": file,
         "ctags": tags
         # TODO source tags 
        })
    









