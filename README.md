# Gulon: Approximate Nearest Neighbours

[![Build status](https://img.shields.io/travis/tixxit/gulon/master.svg)](https://travis-ci.org/tixxit/gulon)
[![Coverage status](https://img.shields.io/codecov/c/github/tixxit/gulon/master.svg)](https://codecov.io/github/tixxit/gulon)

Build and query small approximate nearest neighbour (ANN) indices on the JVM.

This focuses on building small ANN indices that can be loaded into memory from
large word or other entity embeddings. It is designed to work with O(10m)
vectors and return queries in 5ms or less on modern hardware.
