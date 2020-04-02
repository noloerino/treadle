PID=$(jps | grep sbt-launch | awk '{ print $1 }')
./profiler.sh -d 10 -f flamegraph.svg $PID
    
