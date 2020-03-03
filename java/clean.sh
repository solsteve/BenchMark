#!/bin/bash
if [ "$1" == "fullclean" ]; then
  ant fullclean
  exit 0
fi

if [ "$1" == "clean" ]; then
  ant clean
  exit 0
fi

exit 1
