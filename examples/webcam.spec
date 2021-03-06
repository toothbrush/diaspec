-- This is an example Diaspec scenario.
-- It follows the example in the paper.

-- Taxonomy items (provided by platform) {

source IP      as String
source Camera  as Picture
action Screen  as Picture

-- }
-- Items defined by a developer {

context ProcessPicture as Picture {
    when_provided Camera
    always_publish }

context ComposeDisplay as Picture {
    when_provided ProcessPicture
    get MakeAd
    maybe_publish }

context MakeAd as String {
    when_required
    get IP }

controller Display {
    when_provided ComposeDisplay
    do Screen }

-- }
