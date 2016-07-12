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


def norm(text):
    return "  {}  ".format(text.lower().strip().replace(" and ", " ").replace(" or ", " ").replace("/", " ").replace(".", " ").replace(",", " ").replace("-", " ").replace("_", " "))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
