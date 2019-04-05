module Coal.Snapshot.Skills where

import Coal.Snapshot.Types (Skill(..))
import Data.Array (filter, head)
import Data.Eq ((==))
import Data.Foldable (fold)
import Data.Function (const, ($))
import Data.Maybe (Maybe, maybe)

lookupSkill :: Array Skill -> String -> Maybe Skill
lookupSkill skills name = head $ filter (\(Skill s) -> s.skillName == name) skills

hasSkill :: Array Skill -> String -> Boolean
hasSkill skills name = maybe false (const true) $ lookupSkill skills name

skillHCPermed :: Array Skill -> String -> Boolean
skillHCPermed skills name = maybe false (\(Skill s) -> s.skillHCPerm) $ lookupSkill skills name

buildSkill :: Array Skill -> String -> String -> String
buildSkill skills name wiki = fold [ "<td class=\"", if hc then "greenbox" else if has then "bluebox" else "whitebox", "\">"
                                   , "<a href=\"", wiki, "\"><div class=\"skillname\">", name, "</div></a>"
                                   , "</td>"
                                   ]
  where has = hasSkill skills name
        hc = skillHCPermed skills name

buildClassSkills :: Array Skill -> String
buildClassSkills skills = fold [ "<table>"
                               , buildSCRow1 skills
                               , buildSCRow2 skills
                               , buildTTRow1 skills
                               , buildTTRow2 skills
                               , buildPMRow1 skills
                               , buildPMRow2 skills
                               , buildSARow1 skills
                               , buildSARow2 skills
                               , buildDBRow1 skills
                               , buildDBRow2 skills
                               , buildATRow1 skills
                               , buildATRow2 skills
                               , "</table>" 
                               ]

buildSCRow1 :: Array Skill -> String
buildSCRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Seal Clubbing Frenzy" "http://kol.coldfront.net/thekolwiki/index.php/Seal%20Clubbing%20Frenzy"
                          , buildSkill skills "Fortitude of the Muskox" "http://kol.coldfront.net/thekolwiki/index.php/Fortitude%20of%20the%20Muskox"
                          , buildSkill skills "Audacity of the Otter" "http://kol.coldfront.net/thekolwiki/index.php/Audacity%20of%20the%20Otter"
                          , buildSkill skills "Blubber Up" "http://kol.coldfront.net/thekolwiki/index.php/Blubber%20Up"
                          , buildSkill skills "Thrust-Smack" "http://kol.coldfront.net/thekolwiki/index.php/Thrust-Smack"
                          , buildSkill skills "Super-Advanced Meatsmithing" "http://kol.coldfront.net/thekolwiki/index.php/Super-Advanced%20Meatsmithing"
                          , buildSkill skills "Thirst of the Weasel" "http://kol.coldfront.net/thekolwiki/index.php/Thirst%20of%20the%20Weasel"
                          , buildSkill skills "Hide of the Walrus" "http://kol.coldfront.net/thekolwiki/index.php/Hide%20of%20the%20Walrus"
                          , buildSkill skills "Claws of the Walrus" "http://kol.coldfront.net/thekolwiki/index.php/Claws%20of%20the%20Walrus"
                          , buildSkill skills "Tongue of the Walrus" "http://kol.coldfront.net/thekolwiki/index.php/Tongue%20of%20the%20Walrus"
                          , buildSkill skills "Lunging Thrust-Smack" "http://kol.coldfront.net/thekolwiki/index.php/Lunging%20Thrust-Smack"
                          , buildSkill skills "Rage of the Reindeer" "http://kol.coldfront.net/thekolwiki/index.php/Rage%20of%20the%20Reindeer"
                          , buildSkill skills "Double-Fisted Skull Smashing" "http://kol.coldfront.net/thekolwiki/index.php/Double-Fisted%20Skull%20Smashing"
                          , buildSkill skills "Northern Exposure" "http://kol.coldfront.net/thekolwiki/index.php/Northern%20Exposure"
                          , buildSkill skills "Musk of the Moose" "http://kol.coldfront.net/thekolwiki/index.php/Musk%20of%20the%20Moose"
                          , buildSkill skills "Pulverize" "http://kol.coldfront.net/thekolwiki/index.php/Pulverize"
                          , "</tr>"
                          ]

buildSCRow2 :: Array Skill -> String
buildSCRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Clobber" "http://kol.coldfront.net/thekolwiki/index.php/Clobber"
                          , buildSkill skills "Lunge Smack" "http://kol.coldfront.net/thekolwiki/index.php/Lunge%20Smack"
                          , buildSkill skills "Hibernate" "http://kol.coldfront.net/thekolwiki/index.php/Hibernate"
                          , buildSkill skills "Cold Shoulder" "http://kol.coldfront.net/thekolwiki/index.php/Cold%20Shoulder"
                          , buildSkill skills "Wrath of the Wolverine" "http://kol.coldfront.net/thekolwiki/index.php/Wrath%20of%20the%20Wolverine"
                          , buildSkill skills "Buoyancy of the Beluga" "http://kol.coldfront.net/thekolwiki/index.php/Buoyancy%20of%20the%20Beluga"
                          , buildSkill skills "Scowl of the Auk" "http://kol.coldfront.net/thekolwiki/index.php/Scowl%20of%20the%20Auk%20(skill)"
                          , buildSkill skills "Furious Wallop" "http://kol.coldfront.net/thekolwiki/index.php/Furious%20Wallop"
                          , buildSkill skills "Club Foot" "http://kol.coldfront.net/thekolwiki/index.php/Club%20Foot"
                          , buildSkill skills "Seething of the Snow Leopard" "http://kol.coldfront.net/thekolwiki/index.php/Seething%20of%20the%20Snow%20Leopard"
                          , buildSkill skills "Ire of the Orca" "http://kol.coldfront.net/thekolwiki/index.php/Ire%20of%20the%20Orca"
                          , buildSkill skills "Batter Up!" "http://kol.coldfront.net/thekolwiki/index.php/Batter%20Up!"
                          , buildSkill skills "Cavalcade of Fury" "http://kol.coldfront.net/thekolwiki/index.php/Cavalcade%20of%20Fury"
                          , buildSkill skills "Northern Explosion" "http://kol.coldfront.net/thekolwiki/index.php/Northern%20Explosion"
                          , buildSkill skills "Precision of the Penguin" "http://kol.coldfront.net/thekolwiki/index.php/Precision%20of%20the%20Penguin"
                          , buildSkill skills "Pride of the Puffin" "http://kol.coldfront.net/thekolwiki/index.php/Pride%20of%20the%20Puffin"
                          , "</tr>"
                          ]

buildTTRow1 :: Array Skill -> String
buildTTRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Patience of the Tortoise" "http://kol.coldfront.net/thekolwiki/index.php/Patience%20of%20the%20Tortoise"
                          , buildSkill skills "Headbutt" "http://kol.coldfront.net/thekolwiki/index.php/Headbutt"
                          , buildSkill skills "Skin of the Leatherback" "http://kol.coldfront.net/thekolwiki/index.php/Skin%20of%20the%20Leatherback"
                          , buildSkill skills "Amphibian Sympathy" "http://kol.coldfront.net/thekolwiki/index.php/Amphibian%20Sympathy"
                          , buildSkill skills "Ghostly Shell" "http://kol.coldfront.net/thekolwiki/index.php/Ghostly%20Shell"
                          , buildSkill skills "Armorcraftiness" "http://kol.coldfront.net/thekolwiki/index.php/Armorcraftiness"
                          , buildSkill skills "Tenacity of the Snapper" "http://kol.coldfront.net/thekolwiki/index.php/Tenacity%20of%20the%20Snapper"
                          , buildSkill skills "Kneebutt" "http://kol.coldfront.net/thekolwiki/index.php/Kneebutt"
                          , buildSkill skills "Empathy of the Newt" "http://kol.coldfront.net/thekolwiki/index.php/Empathy%20of%20the%20Newt"
                          , buildSkill skills "Testudinal Teachings" "http://kol.coldfront.net/thekolwiki/index.php/Testudinal%20Teachings"
                          , buildSkill skills "Shieldbutt" "http://kol.coldfront.net/thekolwiki/index.php/Shieldbutt"
                          , buildSkill skills "Wisdom of the Elder Tortoises" "http://kol.coldfront.net/thekolwiki/index.php/Wisdom%20of%20the%20Elder%20Tortoises"
                          , buildSkill skills "Astral Shell" "http://kol.coldfront.net/thekolwiki/index.php/Astral%20Shell"
                          , buildSkill skills "Cold-Blooded Fearlessness" "http://kol.coldfront.net/thekolwiki/index.php/Cold-Blooded%20Fearlessness"
                          , buildSkill skills "Hero of the Half-Shell" "http://kol.coldfront.net/thekolwiki/index.php/Hero%20of%20the%20Half-Shell"
                          , buildSkill skills "Tao of the Terrapin" "http://kol.coldfront.net/thekolwiki/index.php/Tao%20of%20the%20Terrapin"
                          , "</tr>"
                          ]

buildTTRow2 :: Array Skill -> String
buildTTRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Toss" "http://kol.coldfront.net/thekolwiki/index.php/Toss"
                          , buildSkill skills "Spirit Vacation" "http://kol.coldfront.net/thekolwiki/index.php/Spirit%20Vacation"
                          , buildSkill skills "Blessing of the War Snapper" "http://kol.coldfront.net/thekolwiki/index.php/Blessing%20of%20the%20War%20Snapper%20(skill)"
                          , buildSkill skills "Stiff Upper Lip" "http://kol.coldfront.net/thekolwiki/index.php/Stiff%20Upper%20Lip"
                          , buildSkill skills "Pizza Lover" "http://kol.coldfront.net/thekolwiki/index.php/Pizza%20Lover"
                          , buildSkill skills "Shell Up" "http://kol.coldfront.net/thekolwiki/index.php/Shell%20Up"
                          , buildSkill skills "Spirit Snap" "http://kol.coldfront.net/thekolwiki/index.php/Spirit%20Snap"
                          , buildSkill skills "Blessing of She-Who-Was" "http://kol.coldfront.net/thekolwiki/index.php/Blessing%20of%20She-Who-Was"
                          , buildSkill skills "Butts of Steel" "http://kol.coldfront.net/thekolwiki/index.php/Butts%20of%20Steel"
                          , buildSkill skills "Spiky Shell" "http://kol.coldfront.net/thekolwiki/index.php/Spiky%20Shell%20(skill)"
                          , buildSkill skills "Reptilian Fortitude" "http://kol.coldfront.net/thekolwiki/index.php/Reptilian%20Fortitude"
                          , buildSkill skills "Blessing of the Storm Tortoise" "http://kol.coldfront.net/thekolwiki/index.php/Blessing%20of%20the%20Storm%20Tortoise"
                          , buildSkill skills "The Long View" "http://kol.coldfront.net/thekolwiki/index.php/The%20Long%20View"
                          , buildSkill skills "Spirit Boon" "http://kol.coldfront.net/thekolwiki/index.php/Spirit%20Boon"
                          , buildSkill skills "Patient Smile" "http://kol.coldfront.net/thekolwiki/index.php/Patient%20Smile"
                          , buildSkill skills "Turtle Power" "http://kol.coldfront.net/thekolwiki/index.php/Turtle%20Power"
                          , "</tr>"
                          ]

buildPMRow1 :: Array Skill -> String
buildPMRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Manicotti Meditation" "http://kol.coldfront.net/thekolwiki/index.php/Manicotti%20Meditation"
                          , buildSkill skills "Ravioli Shurikens" "http://kol.coldfront.net/thekolwiki/index.php/Ravioli%20Shurikens"
                          , buildSkill skills "Entangling Noodles" "http://kol.coldfront.net/thekolwiki/index.php/Entangling%20Noodles"
                          , buildSkill skills "Lasagna Bandages" "http://kol.coldfront.net/thekolwiki/index.php/Lasagna%20Bandages"
                          , buildSkill skills "Cannelloni Cannon" "http://kol.coldfront.net/thekolwiki/index.php/Cannelloni%20Cannon"
                          , buildSkill skills "Pastamastery" "http://kol.coldfront.net/thekolwiki/index.php/Pastamastery"
                          , buildSkill skills "Springy Fusilli" "http://kol.coldfront.net/thekolwiki/index.php/Springy%20Fusilli"
                          , buildSkill skills "Spirit of Rigatoni" "http://kol.coldfront.net/thekolwiki/index.php/Spirit%20of%20Rigatoni"
                          , buildSkill skills "Stuffed Mortar Shell" "http://kol.coldfront.net/thekolwiki/index.php/Stuffed%20Mortar%20Shell"
                          , buildSkill skills "Spirit of Ravioli" "http://kol.coldfront.net/thekolwiki/index.php/Spirit%20of%20Ravioli"
                          , buildSkill skills "Weapon of the Pastalord" "http://kol.coldfront.net/thekolwiki/index.php/Weapon%20of%20the%20Pastalord"
                          , buildSkill skills "Leash of Linguini" "http://kol.coldfront.net/thekolwiki/index.php/Leash%20of%20Linguini"
                          , buildSkill skills "Cannelloni Cocoon" "http://kol.coldfront.net/thekolwiki/index.php/Cannelloni%20Cocoon"
                          , buildSkill skills "Tolerance of the Kitchen" "http://kol.coldfront.net/thekolwiki/index.php/Tolerance%20of%20the%20Kitchen"
                          , buildSkill skills "Flavour of Magic" "http://kol.coldfront.net/thekolwiki/index.php/Flavour%20of%20Magic"
                          , buildSkill skills "Transcendental Noodlecraft" "http://kol.coldfront.net/thekolwiki/index.php/Transcendental%20Noodlecraft"
                          , "</tr>"
                          ]

buildPMRow2 :: Array Skill -> String
buildPMRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Spaghetti Spear" "http://kol.coldfront.net/thekolwiki/index.php/Spaghetti%20Spear"
                          , buildSkill skills "Utensil Twist" "http://kol.coldfront.net/thekolwiki/index.php/Utensil%20Twist"
                          , buildSkill skills "Transcendent Al Dente" "http://kol.coldfront.net/thekolwiki/index.php/Transcendent%20Al%20Dente"
                          , buildSkill skills "Bind Vampieroghi" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Vampieroghi"
                          , buildSkill skills "Arched Eyebrow of the Archmage" "http://kol.coldfront.net/thekolwiki/index.php/Arched%20Eyebrow%20of%20the%20Archmage"
                          , buildSkill skills "Bind Vermincelli" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Vermincelli"
                          , buildSkill skills "Bringing Up the Rear" "http://kol.coldfront.net/thekolwiki/index.php/Bringing%20Up%20the%20Rear"
                          , buildSkill skills "Bind Angel Hair Wisp" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Angel%20Hair%20Wisp"
                          , buildSkill skills "Shield of the Pastalord" "http://kol.coldfront.net/thekolwiki/index.php/Shield%20of%20the%20Pastalord"
                          , buildSkill skills "Bind Undead Elbow Macaroni" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Undead%20Elbow%20Macaroni"
                          , buildSkill skills "Thrall Unit Tactics" "http://kol.coldfront.net/thekolwiki/index.php/Thrall%20Unit%20Tactics"
                          , buildSkill skills "Bind Penne Dreadful" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Penne%20Dreadful"
                          , buildSkill skills "Subtle and Quick to Anger" "http://kol.coldfront.net/thekolwiki/index.php/Subtle%20and%20Quick%20to%20Anger"
                          , buildSkill skills "Bind Lasagmbie" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Lasagmbie"
                          , buildSkill skills "Wizard Squint" "http://kol.coldfront.net/thekolwiki/index.php/Wizard%20Squint"
                          , buildSkill skills "Bind Spice Ghost" "http://kol.coldfront.net/thekolwiki/index.php/Bind%20Spice%20Ghost"
                          , "</tr>"
                          ]

buildSARow1 :: Array Skill -> String
buildSARow1 skills = fold [ "<tr>"
                          , buildSkill skills "Sauce Contemplation" "http://kol.coldfront.net/thekolwiki/index.php/Sauce%20Contemplation"
                          , buildSkill skills "Curse of Vichyssoise" "http://kol.coldfront.net/thekolwiki/index.php/Curse%20of%20Vichyssoise"
                          , buildSkill skills "Saucy Salve" "http://kol.coldfront.net/thekolwiki/index.php/Saucy%20Salve"
                          , buildSkill skills "Expert Panhandling" "http://kol.coldfront.net/thekolwiki/index.php/Expert%20Panhandling"
                          , buildSkill skills "Elemental Saucesphere" "http://kol.coldfront.net/thekolwiki/index.php/Elemental%20Saucesphere"
                          , buildSkill skills "Advanced Saucecrafting" "http://kol.coldfront.net/thekolwiki/index.php/Advanced%20Saucecrafting"
                          , buildSkill skills "Soul Saucery" "http://kol.coldfront.net/thekolwiki/index.php/Soul%20Saucery"
                          , buildSkill skills "Jalapeño Saucesphere" "http://kol.coldfront.net/thekolwiki/index.php/Jalape%C3%B1o%20Saucesphere"
                          , buildSkill skills "Itchy Curse Finger" "http://kol.coldfront.net/thekolwiki/index.php/Itchy%20Curse%20Finger"
                          , buildSkill skills "Intrinsic Spiciness" "http://kol.coldfront.net/thekolwiki/index.php/Intrinsic%20Spiciness"
                          , buildSkill skills "Antibiotic Saucesphere" "http://kol.coldfront.net/thekolwiki/index.php/Antibiotic%20Saucesphere"
                          , buildSkill skills "Saucegeyser" "http://kol.coldfront.net/thekolwiki/index.php/Saucegeyser"
                          , buildSkill skills "Impetuous Sauciness" "http://kol.coldfront.net/thekolwiki/index.php/Impetuous%20Sauciness"
                          , buildSkill skills "Diminished Gag Reflex" "http://kol.coldfront.net/thekolwiki/index.php/Diminished%20Gag%20Reflex"
                          , buildSkill skills "Irrepressible Spunk" "http://kol.coldfront.net/thekolwiki/index.php/Irrepressible%20Spunk"
                          , buildSkill skills "The Way of Sauce" "http://kol.coldfront.net/thekolwiki/index.php/The%20Way%20of%20Sauce"
                          , "</tr>"
                          ]

buildSARow2 :: Array Skill -> String
buildSARow2 skills = fold [ "<tr>"
                          , buildSkill skills "Salsaball" "http://kol.coldfront.net/thekolwiki/index.php/Salsaball"
                          , buildSkill skills "Simmer" "http://kol.coldfront.net/thekolwiki/index.php/Simmer"
                          , buildSkill skills "Stream of Sauce" "http://kol.coldfront.net/thekolwiki/index.php/Stream%20of%20Sauce"
                          , buildSkill skills "Icy Glare" "http://kol.coldfront.net/thekolwiki/index.php/Icy%20Glare"
                          , buildSkill skills "Inner Sauce" "http://kol.coldfront.net/thekolwiki/index.php/Inner%20Sauce"
                          , buildSkill skills "Saucestorm" "http://kol.coldfront.net/thekolwiki/index.php/Saucestorm"
                          , buildSkill skills "Curse of Marinara" "http://kol.coldfront.net/thekolwiki/index.php/Curse%20of%20Marinara"
                          , buildSkill skills "Wave of Sauce" "http://kol.coldfront.net/thekolwiki/index.php/Wave%20of%20Sauce"
                          , buildSkill skills "Curse of the Thousand Islands" "http://kol.coldfront.net/thekolwiki/index.php/Curse%20of%20the%20Thousand%20Islands"
                          , buildSkill skills "Saucecicle" "http://kol.coldfront.net/thekolwiki/index.php/Saucecicle"
                          , buildSkill skills "Master Saucier" "http://kol.coldfront.net/thekolwiki/index.php/Master%20Saucier"
                          , buildSkill skills "Saucemaven" "http://kol.coldfront.net/thekolwiki/index.php/Saucemaven"
                          , buildSkill skills "Curse of Weaksauce" "http://kol.coldfront.net/thekolwiki/index.php/Curse%20of%20Weaksauce"
                          , buildSkill skills "Wry Smile" "http://kol.coldfront.net/thekolwiki/index.php/Wry%20Smile"
                          , buildSkill skills "Sauce Monocle" "http://kol.coldfront.net/thekolwiki/index.php/Sauce%20Monocle%20(skill)"
                          , buildSkill skills "Blood Sugar Sauce Magic" "http://kol.coldfront.net/thekolwiki/index.php/Blood%20Sugar%20Sauce%20Magic"
                          , "</tr>"
                          ]

buildDBRow1 :: Array Skill -> String
buildDBRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Disco Aerobics" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Aerobics"
                          , buildSkill skills "Disco Eye-Poke" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Eye-Poke"
                          , buildSkill skills "Overdeveloped Sense of Self Preservation" "http://kol.coldfront.net/thekolwiki/index.php/Overdeveloped%20Sense%20of%20Self%20Preservation"
                          , buildSkill skills "Disco Nap" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Nap"
                          , buildSkill skills "Disco Dance II: Electric Boogaloo" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Dance%20II:%20Electric%20Boogaloo"
                          , buildSkill skills "Advanced Cocktailcrafting" "http://kol.coldfront.net/thekolwiki/index.php/Advanced%20Cocktailcrafting"
                          , buildSkill skills "Nimble Fingers" "http://kol.coldfront.net/thekolwiki/index.php/Nimble%20Fingers"
                          , buildSkill skills "Tricky Knifework" "http://kol.coldfront.net/thekolwiki/index.php/Tricky%20Knifework"
                          , buildSkill skills "Mad Looting Skillz" "http://kol.coldfront.net/thekolwiki/index.php/Mad%20Looting%20Skillz"
                          , buildSkill skills "Disco Greed" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Greed"
                          , buildSkill skills "Disco Bravado" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Bravado"
                          , buildSkill skills "Adventurer of Leisure" "http://kol.coldfront.net/thekolwiki/index.php/Adventurer%20of%20Leisure"
                          , buildSkill skills "Ambidextrous Funkslinging" "http://kol.coldfront.net/thekolwiki/index.php/Ambidextrous%20Funkslinging"
                          , buildSkill skills "Heart of Polyester" "http://kol.coldfront.net/thekolwiki/index.php/Heart%20of%20Polyester"
                          , buildSkill skills "Smooth Movement" "http://kol.coldfront.net/thekolwiki/index.php/Smooth%20Movement"
                          , buildSkill skills "Superhuman Cocktailcrafting" "http://kol.coldfront.net/thekolwiki/index.php/Superhuman%20Cocktailcrafting"
                          , "</tr>"
                          ]

buildDBRow2 :: Array Skill -> String
buildDBRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Suckerpunch" "http://kol.coldfront.net/thekolwiki/index.php/Suckerpunch"
                          , buildSkill skills "Deft Hands" "http://kol.coldfront.net/thekolwiki/index.php/Deft%20Hands"
                          , buildSkill skills "Disco Dance of Doom" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Dance%20of%20Doom"
                          , buildSkill skills "Disco State of Mind" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20State%20of%20Mind"
                          , buildSkill skills "Frantic Gyrations" "http://kol.coldfront.net/thekolwiki/index.php/Frantic%20Gyrations"
                          , buildSkill skills "That's Not a Knife" "http://kol.coldfront.net/thekolwiki/index.php/That's%20Not%20a%20Knife"
                          , buildSkill skills "Disco Face Stab" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Face%20Stab"
                          , buildSkill skills "Flashy Dancer" "http://kol.coldfront.net/thekolwiki/index.php/Flashy%20Dancer"
                          , buildSkill skills "Disco Smirk" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Smirk"
                          , buildSkill skills "Knife in the Dark" "http://kol.coldfront.net/thekolwiki/index.php/Knife%20in%20the%20Dark"
                          , buildSkill skills "Disco Shank" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Shank"
                          , buildSkill skills "Disco Dance 3: Back in the Habit" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Dance%203:%20Back%20in%20the%20Habit"
                          , buildSkill skills "Disco Inferno" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Inferno"
                          , buildSkill skills "Disco Fever" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Fever"
                          , buildSkill skills "Sensitive Fingers" "http://kol.coldfront.net/thekolwiki/index.php/Sensitive%20Fingers"
                          , buildSkill skills "Disco Leer" "http://kol.coldfront.net/thekolwiki/index.php/Disco%20Leer"
                          , "</tr>"
                          ]

buildATRow1 :: Array Skill -> String
buildATRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Moxie of the Mariachi" "http://kol.coldfront.net/thekolwiki/index.php/Moxie%20of%20the%20Mariachi"
                          , buildSkill skills "The Moxious Madrigal" "http://kol.coldfront.net/thekolwiki/index.php/The%20Moxious%20Madrigal"
                          , buildSkill skills "The Magical Mojomuscular Melody" "http://kol.coldfront.net/thekolwiki/index.php/The%20Magical%20Mojomuscular%20Melody"
                          , buildSkill skills "Cletus's Canticle of Celerity" "http://kol.coldfront.net/thekolwiki/index.php/Cletus's%20Canticle%20of%20Celerity"
                          , buildSkill skills "The Power Ballad of the Arrowsmith" "http://kol.coldfront.net/thekolwiki/index.php/The%20Power%20Ballad%20of%20the%20Arrowsmith"
                          , buildSkill skills "The Polka of Plenty" "http://kol.coldfront.net/thekolwiki/index.php/The%20Polka%20of%20Plenty"
                          , buildSkill skills "Jackasses' Symphony of Destruction" "http://kol.coldfront.net/thekolwiki/index.php/Jackasses'%20Symphony%20of%20Destruction"
                          , buildSkill skills "Fat Leon's Phat Loot Lyric" "http://kol.coldfront.net/thekolwiki/index.php/Fat%20Leon's%20Phat%20Loot%20Lyric"
                          , buildSkill skills "Brawnee's Anthem of Absorption" "http://kol.coldfront.net/thekolwiki/index.php/Brawnee's%20Anthem%20of%20Absorption"
                          , buildSkill skills "The Psalm of Pointiness" "http://kol.coldfront.net/thekolwiki/index.php/The%20Psalm%20of%20Pointiness"
                          , buildSkill skills "Stevedave's Shanty of Superiority" "http://kol.coldfront.net/thekolwiki/index.php/Stevedave's%20Shanty%20of%20Superiority"
                          , buildSkill skills "Aloysius' Antiphon of Aptitude" "http://kol.coldfront.net/thekolwiki/index.php/Aloysius'%20Antiphon%20of%20Aptitude"
                          , buildSkill skills "The Ode to Booze" "http://kol.coldfront.net/thekolwiki/index.php/The%20Ode%20to%20Booze"
                          , buildSkill skills "The Sonata of Sneakiness" "http://kol.coldfront.net/thekolwiki/index.php/The%20Sonata%20of%20Sneakiness"
                          , buildSkill skills "Carlweather's Cantata of Confrontation" "http://kol.coldfront.net/thekolwiki/index.php/Carlweather's%20Cantata%20of%20Confrontation"
                          , buildSkill skills "Ur-Kel's Aria of Annoyance" "http://kol.coldfront.net/thekolwiki/index.php/Ur-Kel's%20Aria%20of%20Annoyance"
                          , "</tr>"
                          ]

buildATRow2 :: Array Skill -> String
buildATRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Sing" "http://kol.coldfront.net/thekolwiki/index.php/Sing"
                          , buildSkill skills "Dissonant Riff" "http://kol.coldfront.net/thekolwiki/index.php/Dissonant%20Riff"
                          , buildSkill skills "Cadenza" "http://kol.coldfront.net/thekolwiki/index.php/Cadenza"
                          , buildSkill skills "Crab Claw Technique" "http://kol.coldfront.net/thekolwiki/index.php/Crab%20Claw%20Technique"
                          , buildSkill skills "Accordion Bash" "http://kol.coldfront.net/thekolwiki/index.php/Accordion%20Bash"
                          , buildSkill skills "Accordion Appreciation" "http://kol.coldfront.net/thekolwiki/index.php/Accordion%20Appreciation"
                          , buildSkill skills "Concerto de los Muertos" "http://kol.coldfront.net/thekolwiki/index.php/Concerto%20de%20los%20Muertos"
                          , buildSkill skills "Five Finger Discount" "http://kol.coldfront.net/thekolwiki/index.php/Five%20Finger%20Discount"
                          , buildSkill skills "Suspicious Gaze" "http://kol.coldfront.net/thekolwiki/index.php/Suspicious%20Gaze"
                          , buildSkill skills "Bawdy Refrain" "http://kol.coldfront.net/thekolwiki/index.php/Bawdy%20Refrain"
                          , buildSkill skills "Thief Among the Honorable" "http://kol.coldfront.net/thekolwiki/index.php/Thief%20Among%20the%20Honorable"
                          , buildSkill skills "Sticky Fingers" "http://kol.coldfront.net/thekolwiki/index.php/Sticky%20Fingers"
                          , buildSkill skills "Cone of Zydeco" "http://kol.coldfront.net/thekolwiki/index.php/Cone%20of%20Zydeco"
                          , buildSkill skills "Master Accordion Master Thief" "http://kol.coldfront.net/thekolwiki/index.php/Master%20Accordion%20Master%20Thief"
                          , buildSkill skills "Knowing Smile" "http://kol.coldfront.net/thekolwiki/index.php/Knowing%20Smile"
                          , buildSkill skills "Mariachi Memory" "http://kol.coldfront.net/thekolwiki/index.php/Mariachi%20Memory"
                          , "</tr>"
                          ]

buildSpookyravenSkills :: Array Skill -> String
buildSpookyravenSkills skills = fold [ "<table><tr>"
                                     , buildSkill skills "Snarl of the Timberwolf" ""
                                     , buildSkill skills "Spectral Snapper" ""
                                     , buildSkill skills "Fearful Fettucini" ""
                                     , buildSkill skills "Scarysauce" ""
                                     , buildSkill skills "Tango of Terror" ""
                                     , buildSkill skills "Dirge of Dreadfulness" ""
                                     , "</tr></table>" 
                                     ]

buildSeaSkills :: Array Skill -> String
buildSeaSkills skills = fold [ "<table><tr>"
                             , buildSkill skills "Harpoon!" ""
                             , buildSkill skills "Summon Leviatuga" ""
                             , buildSkill skills "Tempuramancy" ""
                             , buildSkill skills "Deep Saucery" ""
                             , buildSkill skills "Salacious Cocktailcrafting" ""
                             , buildSkill skills "Donho's Bubbly Ballad" ""
                             , "</tr></table>" 
                             ]

buildGnomeSkills :: Array Skill -> String
buildGnomeSkills skills = fold [ "<table><tr>"
                                , buildSkill skills "Torso Awaregness" ""
                                , buildSkill skills "Gnomish Hardigness" ""
                                , buildSkill skills "Cosmic Ugnerstanding" ""
                                , buildSkill skills "Powers of Observatiogn" ""
                                , buildSkill skills "Gnefarious Pickpocketing" ""
                                , "</tr></table>" 
                                ]

buildDailyDungeonSkills :: Array Skill -> String
buildDailyDungeonSkills skills = fold [ "<table><tr>"
                                      , buildSkill skills "Singer's Faithful Ocelot" ""
                                      , buildSkill skills "Drescher's Annoying Noise" ""
                                      , buildSkill skills "Walberg's Dim Bulb" ""
                                      , "</tr></table>" 
                                      ]

buildRaffleSkills :: Array Skill -> String
buildRaffleSkills skills = fold [ "<table><tr>"
                                , buildSkill skills "Brain Games" ""
                                , buildSkill skills "20/20 Vision" ""
                                , "</tr></table>" 
                                ]

buildTraderSkills :: Array Skill -> String
buildTraderSkills skills = fold [ "<table><tr>"
                                , buildSkill skills "Rainbow Gravitation" ""
                                , buildSkill skills "Iron Palm Technique" ""
                                , buildSkill skills "Curiosity of Br'er Tarrypin" ""
                                , buildSkill skills "Stringozzi Serpent" ""
                                , buildSkill skills "Käsesoßesturm" ""
                                , buildSkill skills "Kung Fu Hustler" ""
                                , buildSkill skills "Inigo's Incantation of Inspiration" ""
                                , "</tr></table>" 
                                ]

buildCrimboSkills :: Array Skill -> String
buildCrimboSkills skills = fold [ "<table>"
                                , "<tr>"
                                , buildSkill skills "Holiday Weight Gain" ""
                                , buildSkill skills "Jingle Bells" ""
                                , buildSkill skills "Candyblast" ""
                                , buildSkill skills "Surge of Icing" ""
                                , buildSkill skills "Stealth Mistletoe" ""
                                , buildSkill skills "Cringle's Curative Carol" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Fashionably Late" ""
                                , buildSkill skills "Executive Narcolepsy" ""
                                , buildSkill skills "Lunch Break" ""
                                , buildSkill skills "Offensive Joke" ""
                                , buildSkill skills "Managerial Manipulation" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Shrap" ""
                                , buildSkill skills "Psychokinetic Hug" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Rapid Prototyping" ""
                                , buildSkill skills "Mathematical Precision" ""
                                , buildSkill skills "Ruthless Efficiency" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Communism!" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Stack Lumps" ""
                                , buildSkill skills "Sweet Synthesis" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Silent Hunter" ""
                                , buildSkill skills "Quiet Determination" ""
                                , buildSkill skills "Quiet Judgement" ""
                                , buildSkill skills "Silent Treatment" ""
                                , buildSkill skills "Silent Knife" ""
                                , buildSkill skills "Quiet Desperation" ""
                                , "</tr>"
                                , "<tr>"
                                , buildSkill skills "Carol of the Bulls" ""
                                , buildSkill skills "Carol of the Hells" ""
                                , buildSkill skills "Carol of the Thrills" ""
                                , "</tr>"
                                , "</table>" 
                                ]

buildHobopolisSkills :: Array Skill -> String
buildHobopolisSkills skills = fold [ "<table>"
                                   , "<tr>"
                                   , buildSkill skills "Natural Born Scrabbler" ""
                                   , buildSkill skills "Thrift and Grift" ""
                                   , buildSkill skills "Abs of Tin" ""
                                   , buildSkill skills "Marginally Insane" ""
                                   , "</tr>"
                                   , "<tr>"
                                   , buildSkill skills "Conjure Relaxing Campfire" ""
                                   , buildSkill skills "Maximum Chill" ""
                                   , buildSkill skills "Mudbath" ""
                                   , buildSkill skills "Inappropriate Backrub" ""
                                   , buildSkill skills "Creepy Lullaby" ""
                                   , buildSkill skills "Wassail" ""
                                   , "</tr>"
                                   , "<tr>"
                                   , buildSkill skills "Awesome Balls of Fire" ""
                                   , buildSkill skills "Snowclone" ""
                                   , buildSkill skills "Eggsplosion" ""
                                   , buildSkill skills "Grease Lightning" ""
                                   , buildSkill skills "Raise Backup Dancer" ""
                                   , buildSkill skills "Toynado" ""
                                   , "</tr>"
                                   , "<tr>"
                                   , buildSkill skills "The Ballad of Richie Thingfinder" ""
                                   , buildSkill skills "Benetton's Medley of Diversity" ""
                                   , buildSkill skills "Elron's Explosive Etude" ""
                                   , buildSkill skills "Chorale of Companionship" ""
                                   , buildSkill skills "Prelude of Precision" ""
                                   , "</tr>"
                                   , "</table>" 
                                   ]

buildSlimeSkills :: Array Skill -> String
buildSlimeSkills skills = fold [ "<table><tr>"
                               , buildSkill skills "Slimy Sinews" ""
                               , buildSkill skills "Slimy Synapses" ""
                               , buildSkill skills "Slimy Shoulders" ""
                               , "</tr></table>" 
                               ]

buildDreadSkills :: Array Skill -> String
buildDreadSkills skills = fold [ "<table><tr>"
                               , buildSkill skills "Club Earth" ""
                               , buildSkill skills "Carbohydrate Cudgel" ""
                               , buildSkill skills "Splattersmash" ""
                               , buildSkill skills "Grab a Cold One" ""
                               , buildSkill skills "Song of the North" ""
                               , buildSkill skills "Turtleini" ""
                               , buildSkill skills "Sauceshell" ""
                               , buildSkill skills "Conspiratorial Whispers" ""
                               , buildSkill skills "Song of Slowness" ""
                               , buildSkill skills "Spaghetti Breakfast" ""
                               , buildSkill skills "Shadow Noodles" ""
                               , buildSkill skills "Song of Starch" ""
                               , buildSkill skills "Splashdance" ""
                               , buildSkill skills "Song of Sauce" ""
                               , buildSkill skills "Song of Bravado" ""
                               , "</tr></table>" 
                               ]

buildPVPSkills :: Array Skill -> String
buildPVPSkills skills = fold [ "<table><tr>"
                               , buildSkill skills "Thick-Skinned" ""
                               , buildSkill skills "Chip on your Shoulder" ""
                               , buildSkill skills "Summon Holiday Fun!" ""
                               , buildSkill skills "Summon Carrot" ""
                               , buildSkill skills "Summon Kokomo Resort Pass" ""
                               , buildSkill skills "Bear Essence" ""
                               , buildSkill skills "Pirate Bellow" ""
                               , buildSkill skills "Summon Annoyance" ""
                               , buildSkill skills "Calculate the Universe" ""
                               , buildSkill skills "Toggle Optimality" ""
                               , buildSkill skills "Experience Safari" ""
                               , "</tr></table>" 
                               ]

buildAscensionSkills :: Array Skill -> String
buildAscensionSkills skills = fold [ "<table><tr>"
                                   , buildSkill skills "Master of the Surprising Fist" ""
                                   , buildSkill skills "Request Sandwich" ""
                                   , buildSkill skills "Mild Curse" ""
                                   , buildSkill skills "Belch The Rainbow" ""
                                   , "</tr></table>" 
                                   ]

buildMimeSkills :: Array Skill -> String
buildMimeSkills skills = fold [ "<table><tr>"
                              , buildSkill skills "Silent Slam" ""
                              , buildSkill skills "Silent Squirt" ""
                              , buildSkill skills "Silent Slice" ""
                              , "</tr></table>" 
                              ]

buildDisSkills :: Array Skill -> String
buildDisSkills skills = fold [ "<table><tr>"
                             , buildSkill skills "Torment Plant" ""
                             , buildSkill skills "Pinch Ghost" ""
                             , buildSkill skills "Tattle" ""
                             , "</tr></table>" 
                             ]

buildAirportSkills :: Array Skill -> String
buildAirportSkills skills = fold [ "<table>"
                                 , "<tr>"
                                 , buildSkill skills "Grease Up" ""
                                 , buildSkill skills "Sloppy Secrets" ""
                                 , buildSkill skills "Unoffendable" ""
                                 , "</tr>"
                                 , "<tr>"
                                 , buildSkill skills "Hypersane" ""
                                 , buildSkill skills "Intimidating Mien" ""
                                 , "</tr>"
                                 , "<tr>"
                                 , buildSkill skills "Olfactory Burnout" ""
                                 , buildSkill skills "Garbage Nova" ""
                                 , buildSkill skills "Dinsey Operations Expert" ""
                                 , buildSkill skills "Rotten Memories" ""
                                 , "</tr>"
                                 , "<tr>"
                                 , buildSkill skills "Asbestos Heart" ""
                                 , buildSkill skills "Firegate" ""
                                 , buildSkill skills "Pyromania" ""
                                 , "</tr>"
                                 , "<tr>"
                                 , buildSkill skills "Beardfreeze" ""
                                 , buildSkill skills "Frost Bite" ""
                                 , buildSkill skills "Perfect Freeze" ""
                                 , buildSkill skills "Refusal to Freeze" ""
                                 , "</tr>"
                                 , "</table>" 
                                 ]

buildLTTSkills :: Array Skill -> String
buildLTTSkills skills = fold [ "<table><tr>"
                             , buildSkill skills "Bow-Legged Swagger" ""
                             , buildSkill skills "Bend Hell" ""
                             , buildSkill skills "Steely-Eyed Squint" ""
                             , "</tr></table>" 
                             ]

buildDeckSkills :: Array Skill -> String
buildDeckSkills skills = fold [ "<table><tr>"
                              , buildSkill skills "Ancestral Recall" ""
                              , buildSkill skills "Dark Ritual" ""
                              , buildSkill skills "Giant Growth" ""
                              , buildSkill skills "Healing Salve" ""
                              , buildSkill skills "Lightning Bolt" ""
                              , "</tr></table>" 
                              ]

buildSnojoSkills :: Array Skill -> String
buildSnojoSkills skills = fold [ "<table><tr>"
                               , buildSkill skills "Shattering Punch" ""
                               , buildSkill skills "Shivering Monkey Technique" ""
                               , buildSkill skills "Snokebomb" ""
                               , "</tr></table>" 
                               ]

buildGingerbreadSkills :: Array Skill -> String
buildGingerbreadSkills skills = fold [ "<table><tr>"
                                     , buildSkill skills "Licorice Rope" ""
                                     , buildSkill skills "Gingerbread Mob Hit" ""
                                     , buildSkill skills "Fifteen Minute of Flame" ""
                                     , buildSkill skills "Ceci N'Est Pas Un Chapeau" ""
                                     , "</tr></table>" 
                                     ]

buildSpacegateSkills :: Array Skill -> String
buildSpacegateSkills skills = fold [ "<table><tr>"
                                   , buildSkill skills "Quantum Movement" ""
                                   , buildSkill skills "5-D Earning Potential" ""
                                   , buildSkill skills "Object Quasi-Permanence" ""
                                   , buildSkill skills "Disintegrate" ""
                                   , "</tr></table>" 
                                   ]

buildZataraSkills :: Array Skill -> String
buildZataraSkills skills = fold [ "<table><tr>"
                                , buildSkill skills "Get Big" ""
                                , buildSkill skills "Gallapagosian Mating Call" ""
                                , buildSkill skills "Inscrutable Gaze" ""
                                , buildSkill skills "Love Mixology" ""
                                , buildSkill skills "Acquire Rhinestones" ""
                                , buildSkill skills "Paul's Passionate Pop Song" ""
                                , "</tr></table>" 
                                ]

buildPartySkills :: Array Skill -> String
buildPartySkills skills = fold [ "<table><tr>"
                               , buildSkill skills "Budget Conscious" ""
                               , buildSkill skills "Drinking to Drink" ""
                               , "</tr></table>" 
                               ]

buildEldritchSkills :: Array Skill -> String
buildEldritchSkills skills = fold [ "<table><tr>"
                                  , buildSkill skills "Eldritch Intellect" ""
                                  , buildSkill skills "Evoke Eldritch Horror" ""
                                  , buildSkill skills "Eternal Flame" ""
                                  , "</tr></table>" 
                                  ]

buildMiscSkills :: Array Skill -> String
buildMiscSkills skills = fold [ "<table><tr>"
                              , buildSkill skills "Chronic Indigestion" ""
                              , buildSkill skills "CLEESH" ""
                              , buildSkill skills "Really Expensive Jewelrycrafting" ""
                              , buildSkill skills "Transcendent Olfaction" ""
                              , buildSkill skills "Vent Rage Gland" ""
                              , buildSkill skills "Summon \"Boner Battalion\"" ""
                              , buildSkill skills "Summon Crimbo Candy" ""
                              , buildSkill skills "Unaccompanied Miner" ""
                              , buildSkill skills "Volcanometeor Showeruption" ""
                              , buildSkill skills "Natural Born Skeleton Killer" ""
                              , buildSkill skills "Deep Dark Visions" ""
                              , buildSkill skills "Frigidalmatian" ""
                              , buildSkill skills "Bind Spaghetti Elemental" ""
                              , buildSkill skills "Speluck" ""
                              , buildSkill skills "Astute Angler" ""
                              , buildSkill skills "Expert Corner-Cutter" ""
                              , buildSkill skills "Army of Toddlers" ""
                              , "</tr></table>" 
                              ]
