# flying-cats
cats effects implementations of fly.io ditributed systems challenges

## build and run
native executable can be built by calling:
```
sbt nativeImage
```

The build for each module can then be found under `modules/<module-name>/target/native-image/<module-name>`.

## installing maelstrom

```
# install dependencies (JDK required but already installed)
sudo apt install graphviz gnuplot

# get maelstrom and unzip
wget https://github.com/jepsen-io/maelstrom/releases/download/v0.2.3/maelstrom.tar.bz2
tar -xf maelstrom.tar.bz2
```

## running stages

echo:
```
./maelstrom/maelstrom test -w echo --bin ~/Documents/projects/flying-cats/modules/flying-cats-echo/target/native-image/flying-cats-echo --node-count 1 --time-limit 10
```

generate-uid:
```
./maelstrom/maelstrom test -w unique-ids --bin ~/Documents/projects/flying-cats/modules/flying-cats-uid/target/native-image/flying-cats-uid --time-limit 30 --rate 1000 --node-count 3 --availability total --nemesis partition
```

broadcast 3-a
```
./maelstrom/maelstrom test -w broadcast --bin ~/Documents/projects/flying-cats/modules/flying-cats-broadcast/target/native-image/flying-cats-broadcast --node-count 1 --time-limit 20 --rate 10
```

broadcast 3-b
```
./maelstrom/maelstrom test -w broadcast --bin ~/Documents/projects/flying-cats/modules/flying-cats-broadcast/target/native-image/flying-cats-broadcast --node-count 5 --time-limit 20 --rate 10
```

broadcast 3-c
```
./maelstrom/maelstrom test -w broadcast --bin ~/Documents/projects/flying-cats/modules/flying-cats-broadcast/target/native-image/flying-cats-broadcast --node-count 5 --time-limit 20 --rate 10 --nemesis partition
```

broadcast 3-d
```

 ./maelstrom/maelstrom test -w broadcast --bin ~/Documents/projects/flying-cats/modules/flying-cats-broadcast/target/native-image/flying-cats-broadcast --node-count 25 --time-limit 20 --rate 100 --latency 100
```


### example messages
init
```
{"src": "c1", "dest": "n3", "body": {"type": "init",  "msg_id":   1,  "node_id":  "n3",  "node_ids": ["n1", "n2", "n3"]}}
```

read
```
{"src": "c1", "dest": "n3", "body": {"type": "read"}}
```

broadcast
```
{"src": "c1", "dest": "n3", "body": {"type": "broadcast", "message": 1000}}
```

topology
```

{"src": "c1", "dest": "n3", "body": {"type": "topology","topology": {"n1": ["n2", "n3"],"n2": ["n1"],"n3": ["n1"]}}}
```
