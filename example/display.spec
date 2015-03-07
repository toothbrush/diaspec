action Screen as Picture


context ComposeDisplay as Picture {
    when_provided Camera always_publish }

controller Display {
    when_provided ComposeDisplay
    do Screen }

source Camera as Int
