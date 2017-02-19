#!/bin/bash


tmux splitw 'stack exec fastchain -- -c config1.json'
tmux splitw -h 'stack exec fastchain -- -c config2.json'
tmux splitw -h -t 1 'stack exec fastchain -- -c config3.json'
stack exec fastchain -- -c config0.json --testClient
