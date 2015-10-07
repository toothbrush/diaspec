-- does this work??
source Camera as Picture
context B as String {
    when_required }

context A as Int {
    when_required get B }

context C as Picture {
    when_provided Camera get A always_publish }

