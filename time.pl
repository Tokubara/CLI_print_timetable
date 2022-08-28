BEGIN {
    print("class_time=c(");
}
END {
    print(")\n");
}
while(<>) {
    chomp;
    print("\"$.\"=\"$_\",")
}
