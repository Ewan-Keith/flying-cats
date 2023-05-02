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

