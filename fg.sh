PID=$(jps | grep sbt-launch | awk '{ print $1 }')
./profiler.sh -d 5 -f flamegraph.svg $PID
    
