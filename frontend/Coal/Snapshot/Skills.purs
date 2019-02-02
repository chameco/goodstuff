module Coal.Snapshot.Skills where

import Coal.Snapshot.Types (Skill(..))
import Data.Array (filter, head)
import Data.Eq ((==))
import Data.Foldable (fold)
import Data.Function (const, ($))
import Data.Maybe (Maybe, maybe)

buildSkills :: Array Skill -> String
buildSkills skills = fold [ "<table>"
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

buildSCRow1 :: Array Skill -> String
buildSCRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Seal Clubbing Frenzy" ""
                          , buildSkill skills "Fortitude of the Muskox" ""
                          , buildSkill skills "Audacity of the Otter" ""
                          , buildSkill skills "Blubber Up" ""
                          , buildSkill skills "Thrust-Smack" ""
                          , buildSkill skills "Super-Advanced Meatsmithing" ""
                          , buildSkill skills "Thirst of the Weasel" ""
                          , buildSkill skills "Hide of the Walrus" ""
                          , buildSkill skills "Claws of the Walrus" ""
                          , buildSkill skills "Tongue of the Walrus" ""
                          , buildSkill skills "Lunging Thrust-Smack" ""
                          , buildSkill skills "Rage of the Reindeer" ""
                          , buildSkill skills "Double-Fisted Skull Smashing" ""
                          , buildSkill skills "Northern Exposure" ""
                          , buildSkill skills "Musk of the Moose" ""
                          , buildSkill skills "Pulverize" ""
                          , "</tr>"
                          ]

buildSCRow2 :: Array Skill -> String
buildSCRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Clobber" ""
                          , buildSkill skills "Lunge Smack" ""
                          , buildSkill skills "Hibernate" ""
                          , buildSkill skills "Cold Shoulder" ""
                          , buildSkill skills "Wrath of the Wolverine" ""
                          , buildSkill skills "Buoyancy of the Beluga" ""
                          , buildSkill skills "Scowl of the Auk" ""
                          , buildSkill skills "Furious Wallop" ""
                          , buildSkill skills "Club Foot" ""
                          , buildSkill skills "Seething of the Snow Leopard" ""
                          , buildSkill skills "Ire of the Orca" ""
                          , buildSkill skills "Batter Up!" ""
                          , buildSkill skills "Cavalcade of Fury" ""
                          , buildSkill skills "Northern Explosion" ""
                          , buildSkill skills "Precision of the Penguin" ""
                          , buildSkill skills "Pride of the Puffin" ""
                          , "</tr>"
                          ]

buildTTRow1 :: Array Skill -> String
buildTTRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Patience of the Tortoise" ""
                          , buildSkill skills "Headbutt" ""
                          , buildSkill skills "Skin of the Leatherback" ""
                          , buildSkill skills "Amphibian Sympathy" ""
                          , buildSkill skills "Ghostly Shell" ""
                          , buildSkill skills "Armorcraftiness" ""
                          , buildSkill skills "Tenacity of the Snapper" ""
                          , buildSkill skills "Kneebutt" ""
                          , buildSkill skills "Empathy of the Newt" ""
                          , buildSkill skills "Testudinal Teachings" ""
                          , buildSkill skills "Shieldbutt" ""
                          , buildSkill skills "Wisdom of the Elder Tortoises" ""
                          , buildSkill skills "Astral Shell" ""
                          , buildSkill skills "Cold-Blooded Fearlessness" ""
                          , buildSkill skills "Hero of the Half-Shell" ""
                          , buildSkill skills "Tao of the Terrapin" ""
                          , "</tr>"
                          ]

buildTTRow2 :: Array Skill -> String
buildTTRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Toss" ""
                          , buildSkill skills "Spirit Vacation" ""
                          , buildSkill skills "Blessing of the War Snapper" ""
                          , buildSkill skills "Stiff Upper Lip" ""
                          , buildSkill skills "Pizza Lover" ""
                          , buildSkill skills "Shell Up" ""
                          , buildSkill skills "Spirit Snap" ""
                          , buildSkill skills "Blessing of She-Who-Was" ""
                          , buildSkill skills "Butts of Steel" ""
                          , buildSkill skills "Spiky Shell" ""
                          , buildSkill skills "Reptilian Fortitude" ""
                          , buildSkill skills "Blessing of the Storm Tortoise" ""
                          , buildSkill skills "The Long View" ""
                          , buildSkill skills "Spirit Boon" ""
                          , buildSkill skills "Patient Smile" ""
                          , buildSkill skills "Turtle Power" ""
                          , "</tr>"
                          ]

buildPMRow1 :: Array Skill -> String
buildPMRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Manicotti Meditation" ""
                          , buildSkill skills "Ravioli Shurikens" ""
                          , buildSkill skills "Entangling Noodles" ""
                          , buildSkill skills "Lasagna Bandages" ""
                          , buildSkill skills "Cannelloni Cannon" ""
                          , buildSkill skills "Pastamastery" ""
                          , buildSkill skills "Springy Fusilli" ""
                          , buildSkill skills "Spirit of Rigatoni" ""
                          , buildSkill skills "Stuffed Mortar Shell" ""
                          , buildSkill skills "Spirit of Ravioli" ""
                          , buildSkill skills "Weapon of the Pastalord" ""
                          , buildSkill skills "Leash of Linguini" ""
                          , buildSkill skills "Cannelloni Cocoon" ""
                          , buildSkill skills "Tolerance of the Kitchen" ""
                          , buildSkill skills "Flavour of Magic" ""
                          , buildSkill skills "Transcendental Noodlecraft" ""
                          , "</tr>"
                          ]

buildPMRow2 :: Array Skill -> String
buildPMRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Spaghetti Spear" ""
                          , buildSkill skills "Utensil Twist" ""
                          , buildSkill skills "Transcendent Al Dente" ""
                          , buildSkill skills "Bind Vampieroghi" ""
                          , buildSkill skills "Arched Eyebrow of the Archmage" ""
                          , buildSkill skills "Bind Vermincelli" ""
                          , buildSkill skills "Bringing Up the Rear" ""
                          , buildSkill skills "Bind Angel Hair Wisp" ""
                          , buildSkill skills "Shield of the Pastalord" ""
                          , buildSkill skills "Bind Undead Elbow Macaroni" ""
                          , buildSkill skills "Thrall Unit Tactics" ""
                          , buildSkill skills "Bind Penne Dreadful" ""
                          , buildSkill skills "Subtle and Quick to Anger" ""
                          , buildSkill skills "Bind Lasagmbie" ""
                          , buildSkill skills "Wizard Squint" ""
                          , buildSkill skills "Bind Spice Ghost" ""
                          , "</tr>"
                          ]

buildSARow1 :: Array Skill -> String
buildSARow1 skills = fold [ "<tr>"
                          , buildSkill skills "Sauce Contemplation" ""
                          , buildSkill skills "Curse of Vichyssoise" ""
                          , buildSkill skills "Saucy Salve" ""
                          , buildSkill skills "Expert Panhandling" ""
                          , buildSkill skills "Elemental Saucesphere" ""
                          , buildSkill skills "Advanced Saucecrafting" ""
                          , buildSkill skills "Soul Saucery" ""
                          , buildSkill skills "Jalapeño Saucesphere" ""
                          , buildSkill skills "Itchy Curse Finger" ""
                          , buildSkill skills "Intrinsic Spiciness" ""
                          , buildSkill skills "Antibiotic Saucesphere" ""
                          , buildSkill skills "Saucegeyser" ""
                          , buildSkill skills "Impetuous Sauciness" ""
                          , buildSkill skills "Diminished Gag Reflex" ""
                          , buildSkill skills "Irrepressible Spunk" ""
                          , buildSkill skills "The Way of Sauce" ""
                          , "</tr>"
                          ]

buildSARow2 :: Array Skill -> String
buildSARow2 skills = fold [ "<tr>"
                          , buildSkill skills "Salsaball" ""
                          , buildSkill skills "Simmer" ""
                          , buildSkill skills "Stream of Sauce" ""
                          , buildSkill skills "Icy Glare" ""
                          , buildSkill skills "Inner Sauce" ""
                          , buildSkill skills "Saucestorm" ""
                          , buildSkill skills "Curse of Marinara" ""
                          , buildSkill skills "Wave of Sauce" ""
                          , buildSkill skills "Curse of the Thousand Islands" ""
                          , buildSkill skills "Saucecicle" ""
                          , buildSkill skills "Master Saucier" ""
                          , buildSkill skills "Saucemaven" ""
                          , buildSkill skills "Curse of Weaksauce" ""
                          , buildSkill skills "Wry Smile" ""
                          , buildSkill skills "Sauce Monocle" ""
                          , buildSkill skills "Blood Sugar Sauce Magic" ""
                          , "</tr>"
                          ]

buildDBRow1 :: Array Skill -> String
buildDBRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Disco Aerobics" ""
                          , buildSkill skills "Disco Eye-Poke" ""
                          , buildSkill skills "Overdeveloped Sense of Self Preservation" ""
                          , buildSkill skills "Disco Nap" ""
                          , buildSkill skills "Disco Dance II: Electric Boogaloo" ""
                          , buildSkill skills "Advanced Cocktailcrafting" ""
                          , buildSkill skills "Nimble Fingers" ""
                          , buildSkill skills "Tricky Knifework" ""
                          , buildSkill skills "Mad Looting Skillz" ""
                          , buildSkill skills "Disco Greed" ""
                          , buildSkill skills "Disco Bravado" ""
                          , buildSkill skills "Adventurer of Leisure" ""
                          , buildSkill skills "Ambidextrous Funkslinging" ""
                          , buildSkill skills "Heart of Polyester" ""
                          , buildSkill skills "Smooth Movement" ""
                          , buildSkill skills "Superhuman Cocktailcrafting" ""
                          , "</tr>"
                          ]

buildDBRow2 :: Array Skill -> String
buildDBRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Suckerpunch" ""
                          , buildSkill skills "Deft Hands" ""
                          , buildSkill skills "Disco Dance of Doom" ""
                          , buildSkill skills "Disco State of Mind" ""
                          , buildSkill skills "Frantic Gyrations" ""
                          , buildSkill skills "That's Not a Knife" ""
                          , buildSkill skills "Disco Face Stab" ""
                          , buildSkill skills "Flashy Dancer" ""
                          , buildSkill skills "Disco Smirk" ""
                          , buildSkill skills "Knife in the Dark" ""
                          , buildSkill skills "Disco Shank" ""
                          , buildSkill skills "Disco Dance 3: Back in the Habit" ""
                          , buildSkill skills "Disco Inferno" ""
                          , buildSkill skills "Disco Fever" ""
                          , buildSkill skills "Sensitive Fingers" ""
                          , buildSkill skills "Disco Leer" ""
                          , "</tr>"
                          ]

buildATRow1 :: Array Skill -> String
buildATRow1 skills = fold [ "<tr>"
                          , buildSkill skills "Moxie of the Mariachi" ""
                          , buildSkill skills "The Moxious Madrigal" ""
                          , buildSkill skills "The Magical Mojomuscular Melody" ""
                          , buildSkill skills "Cletus's Canticle of Celerity" ""
                          , buildSkill skills "The Power Ballad of the Arrowsmith" ""
                          , buildSkill skills "The Polka of Plenty" ""
                          , buildSkill skills "Jackasses' Symphony of Destruction" ""
                          , buildSkill skills "Fat Leon's Phat Loot Lyric" ""
                          , buildSkill skills "Brawnee's Anthem of Absorption" ""
                          , buildSkill skills "The Psalm of Pointiness" ""
                          , buildSkill skills "Stevedave's Shanty of Superiority" ""
                          , buildSkill skills "Aloysius' Antiphon of Aptitude" ""
                          , buildSkill skills "The Ode to Booze" ""
                          , buildSkill skills "The Sonata of Sneakiness" ""
                          , buildSkill skills "Carlweather's Cantata of Confrontation" ""
                          , buildSkill skills "Ur-Kel's Aria of Annoyance" ""
                          , "</tr>"
                          ]

buildATRow2 :: Array Skill -> String
buildATRow2 skills = fold [ "<tr>"
                          , buildSkill skills "Sing" ""
                          , buildSkill skills "Dissonant Riff" ""
                          , buildSkill skills "Cadenza" ""
                          , buildSkill skills "Crab Claw Technique" ""
                          , buildSkill skills "Accordion Bash" ""
                          , buildSkill skills "Accordion Appreciation" ""
                          , buildSkill skills "Concerto de los Muertos" ""
                          , buildSkill skills "Five Finger Discount" ""
                          , buildSkill skills "Suspicious Gaze" ""
                          , buildSkill skills "Bawdy Refrain" ""
                          , buildSkill skills "Thief Among the Honorable" ""
                          , buildSkill skills "Sticky Fingers" ""
                          , buildSkill skills "Cone of Zydeco" ""
                          , buildSkill skills "Master Accordion Master Thief" ""
                          , buildSkill skills "Knowing Smile" ""
                          , buildSkill skills "Mariachi Memory" ""
                          , "</tr>"
                          ]
