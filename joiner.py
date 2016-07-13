#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys


def main(label_file, synonym_file):
    synonyms = dict()
    for line in open(synonym_file, "r"):
        gid, syn = line.strip().split("|")
        synonyms[gid] = norm(syn)

    for line in open(label_file, "r"):
        parts = line.strip().split("|")
        oid, label, gids = parts
        label = norm(label)
        if gids:
            gids = gids.split(",")
            print("{}|{}|{}".format(oid, label, "|".join(synonyms[gid] for gid in gids)))
        else:
            print("{}|{}".format(oid, label))


to_remove = ["_", "-", ".", ",", " and ", " or ", " of ", "/", " to ", " for ", " at ", " the "]


def norm(text):
    text = text.lower()
    for r in to_remove:
        text = text.replace(r, " ")
        text = text.replace("  ", " ")
    text = text.strip()
    return "  {}  ".format(text)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
