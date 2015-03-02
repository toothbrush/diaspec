source Camera as Picture

source IP as String

action Display as Picture

context ProcessPicture as Picture {
    when_provided Camera always_publish }

context ComposeDisplay as Picture {
    when_provided ProcessPicture maybe_publish }

context MakeAd as String { when_required get IP }

controller Display { when_provided ComposeDisplay do Screen }
