source Camera as Picture
context MakeAd as String {
    when_required get IP }
source IP as String
context ProcessPicture as Picture {
    when_provided Camera
    always_publish }
context ComposeDisplay as Picture {
    when_provided ProcessPicture
    get MakeAd maybe_publish }
