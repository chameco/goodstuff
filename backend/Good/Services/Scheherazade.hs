module Good.Services.Scheherazade where

import Good.Prelude

data Locale = Bed | CoffeeShop
            deriving (Show, Eq)

data Passage = ExhibitAttributes Locale Character
             | ExhibitDesire Locale Character Character
             deriving (Show, Eq)

data Character = Character { characterName :: Text
                           , characterArchetype :: CharacterType
                           , characterPhysicalAttributes :: [CharacterPhysicalAttribute]
                           , characterMentalAttributes :: [CharacterMentalAttribute]
                           , characterSocialAttributes :: [CharacterSocialAttribute]
                           } deriving (Show, Eq)

data CharacterType = CharacterActive | CharacterPassive
                   deriving (Show, Eq)

data CharacterPhysicalAttribute = Slender | Thick | Flat | Busty
                                | Jawline | Pectorals | Biceps | Triceps | Calves
                                deriving (Show, Eq)

data CharacterMentalAttribute = Innocent | Lustful
                              | Intelligent | Cold | Hotblooded
                              deriving (Show, Eq)

data CharacterSocialAttribute = College | Retail | Widow
                              | Biker | Cowboy | Criminal | Doctor | Firefighter | Pirate | Royalty | Spy | Viking | Wealthy
                              deriving (Show, Eq)

data Story = Story { storyExposition :: Exposition
                   , storyAction :: Action
                   , storyClimax :: Climax
                   , storyResolution :: Resolution
                   } deriving (Show, Eq)

data Exposition = Exposition { expositionIntroductions :: [Introduction]
                             , expositionMeetings :: [Meeting]
                             } deriving (Show, Eq)

data Introduction = Introduction { introductionCharacter :: Character
                                 , introductionType :: IntroductionType
                                 } deriving (Show, Eq)

data IntroductionType = IntroductionAwakening | IntroductionActive
                      deriving (Show, Eq)

data Meeting = Meeting { meetingFirstCharacter :: Character
                       , meetingSecondCharacter :: Character
                       , meetingType :: MeetingType
                       } deriving (Show, Eq)

data MeetingType = MeetingCollege | MeetingRetail
                 deriving (Show, Eq)

data Action = Action
            deriving (Show, Eq)

data Climax = Climax
            deriving (Show, Eq)

data Resolution = Resolution
                deriving (Show, Eq)
