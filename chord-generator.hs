import Control.Monad (when)
import Data.List (findIndex, sort, sortBy)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)

-- TYPY --

-- hudební nástroj: iName = název nástroje, iStrings = seznam strun, frets = počet používaných pražců
-- seznam strun sestává z čísel, každé z nich odpovídá tónu, kterým zní prázdná struna
data Instrument = Instrument {iName :: String, iStrings :: [Int], frets :: Int} deriving (Show, Eq)

-- akord: base = základní tón (C -> 0, C# -> 1 apod.), bass = basový tón (volitelně), structure = struktura akordu (popisuje typ akordu)
-- typ (struktura) akordu je určen seznamem tónů, které se v akordu vyskytují (vyjma basového tónu), uvádějí se (relativní) vzdálenosti od tóniky
data Chord = Chord {base :: Int, bass :: Maybe Int, structure :: [Int]} deriving (Show, Eq)

-- hmat/diagram: dStrings = seznam stavů strun, barre = 0 (pro akord bez barré) nebo číslo pražce s barré
data Diagram = Diagram {dStrings :: [StringState], barre :: Int} deriving (Show, Eq)

-- stav struny: Muted (struna nehraje), 0 (struna je prázdná) nebo číslo pražce, na němž se má struna stisknout
data StringState = Muted | Playing Int deriving (Show, Eq)

-- prst: fFret = číslo pražce, fString = číslo struny (počítáno od jedné), fFinger = číslo prstu (počítáno od jedné)
data Finger = Finger {fFret :: Int, fString :: Int, fFinger :: Int} deriving (Show, Eq)

-- TESTOVACÍ DATA --

dataGuitar = Instrument {iName = "kytara", iStrings = [28, 33, 38, 43, 47, 52], frets = 10}

dataUkulele = Instrument {iName = "ukulele", iStrings = [55, 48, 52, 57], frets = 19}

dataCChord = Chord {base = 0, bass = Nothing, structure = [0, 4, 7]}

dataEChord = Chord {base = 4, bass = Nothing, structure = [0, 4, 7]}

dataDm7EChord = Chord {base = 2, bass = Just 4, structure = [0, 3, 7, 10]}

dataEDiagram = Diagram {dStrings = [Playing 0, Playing 2, Playing 2, Playing 1, Playing 0, Playing 0], barre = 0}

-- POMOCNÉ FUNKCE --

-- dostupný počet prstů
maxFingersCount :: Int
maxFingersCount = 4

-- převod tónu do základního rozsahu 0-11
basicNote :: Int -> Int
basicNote = (`mod` 12)

-- vrací strukturu akordu včetně basu
chordStructure :: Chord -> [Int]
chordStructure chord = addBass (structure chord) (base chord) (bass chord)

-- přidá basový tón do struktury akordu, pokud tam ještě není
-- přijímá strukturu akordu, základní tón a basový tón, vrací novou strukturu
addBass :: [Int] -> Int -> Maybe Int -> [Int]
addBass chordStr _ Nothing = chordStr
addBass chordStr base (Just bass)
  | basicNote (bass - base) `elem` chordStr = chordStr
  | otherwise = basicNote (bass - base) : chordStr

-- vrací seznam všech tónů v akordu
chordNotes :: Chord -> [Int]
chordNotes chord = map (basicNote . (+ base chord)) (chordStructure chord)

-- převod stavu struny na číslo znějícího tónu (v základním rozsahu)
noteFromString :: StringState -> Int -> Int
noteFromString (Playing playingString) stringNote = basicNote (playingString + stringNote)

-- rekurzivní převod stavu hmatníku na znějící tóny
-- přijímá seznam strun a seznam jejich stavů, vrací seznam znějících tónů
stringsToNotes :: [Int] -> [StringState] -> [Int]
stringsToNotes _ [] = []
stringsToNotes (stringNote : xs) (stringState : ys)
  | stringState == Muted = stringsToNotes xs ys
  | otherwise = noteFromString stringState stringNote : stringsToNotes xs ys

-- catMaybes pro [StringState]
-- přijímá seznam stavů strun, vrací seznam všech znějících strun (odpovídající čísla pražců)
catPlaying :: [StringState] -> [Int]
catPlaying [] = []
catPlaying (Muted : xs) = catPlaying xs
catPlaying (Playing playingString : xs) = playingString : catPlaying xs

-- spočítá použité prsty v akordu
-- přijímá seznam stavů strun a barré pražec, vrací počet použitých prstů
countFingers :: [StringState] -> Int -> Int
countFingers strings barre = (if barre == 0 then 0 else 1) + countWithoutBarre strings
  where
    countWithoutBarre [] = 0
    countWithoutBarre (Muted : xs) = countWithoutBarre xs
    countWithoutBarre (Playing playingString : xs) = (if barre == playingString then 0 else 1) + countWithoutBarre xs

-- GENEROVÁNÍ --

-- generuje seznam prázdných diagramů s barré na pražcích 0-N, kde N je číslo na vstupu
generateBarres :: Int -> [Diagram]
generateBarres frets = [Diagram [] i | i <- [0 .. frets]]

-- nedeterministicky přiřadí pražec jedné struně diagramu
fillString :: Int -> Chord -> Int -> [Diagram] -> [Diagram]
fillString frets chord stringNote diagrams =
  [ Diagram (stringState : strings) barre
    | Diagram strings barre <- diagrams,
      stringState <- Muted : map Playing [barre .. frets],
      stringState == Muted || noteFromString stringState stringNote `elem` chordNotes chord
  ]

-- nedeterministicky přiřadí pražce všem strunám diagramu
fillAllStrings :: Instrument -> Chord -> [Diagram] -> [Diagram]
fillAllStrings instrument chord = foldr (.) id listFillString
  where
    listFillString = map (fillString (frets instrument) chord) (iStrings instrument)

-- KONTROLY --

-- kontroluje, zda je diagram platný
checkDiagram :: Instrument -> Chord -> Diagram -> Bool
checkDiagram instrument chord diagram = all (\check -> check diagram) diagramChecks
  where
    diagramChecks = [checkNotes . dStrings, checkBarre, checkMuted . dStrings, checkFingersCount]

    -- chord has to contain all of the notes
    checkNotes diagramStrings = all (`elem` stringsToNotes (iStrings instrument) diagramStrings) (chordNotes chord)
    -- barre has to be actively used
    checkBarre diagram = (barre diagram == 0) || (barre diagram `elem` catPlaying (dStrings diagram))

    -- allow muted only on one side (and not in the middle)
    checkMuted diagramStrings = mutedBeforePlaying diagramStrings || mutedBeforePlaying (reverse diagramStrings)
    mutedBeforePlaying (Playing playing : Muted : _) = False
    mutedBeforePlaying (_ : xs) = mutedBeforePlaying xs
    mutedBeforePlaying _ = True

    -- at most 4 fingers should be used
    checkFingersCount diagram = countFingers (dStrings diagram) (barre diagram) <= maxFingersCount

-- SKÓRE --

-- přiřadí číselné skóre diagramu (čím menší, tím lepší)
scoreDiagram :: Instrument -> Chord -> Diagram -> (Int, Diagram)
scoreDiagram instrument chord diagram = (penalty, diagram)
  where
    penalty =
      sum
        [ hasBarre * 10,
          barre diagram * 5,
          (1 - hasBass) * noRoot * rootWeight * 25,
          minTone * 15,
          (maxPos - minTone) * 20,
          (lastString - firstString) * (maxPos - minPos) * 10,
          numberOfFingers * 10,
          hasMuted * mutedWeight * 20,
          numberOfMuted * mutedWeight * 30,
          hasBass * badBass * 100,
          hasBarre * tooLittleBarreNotes * 100,
          hasTooManyMuted * 100
        ]

    playingStrings = catPlaying (dStrings diagram)
    nonzeroStrings = filter (/= 0) playingStrings
    safeNonzeroStrings = if null nonzeroStrings then [0] else nonzeroStrings
    lowestTone = basicNote $ minimum $ realTones (iStrings instrument) (dStrings diagram)
    availableStrings = length (iStrings instrument)

    countMuted [] = 0
    countMuted (Muted : xs) = 1 + countMuted xs
    countMuted (Playing _ : xs) = countMuted xs

    realTones [] [] = []
    realTones (_ : xs) (Muted : ys) = realTones xs ys
    realTones (realString : xs) (Playing tone : ys) = (realString + tone) : realTones xs ys

    safeIndexOfFirstHeldString strings = fromMaybe 0 $ findIndex (\x -> (x /= Muted) && (x /= Playing (barre diagram))) strings

    rootWeight = if availableStrings > 4 then 3 else 1
    mutedWeight = if availableStrings > 4 then 1 else 3
    hasBass = if isNothing (bass chord) then 0 else 1
    badBass = if Just lowestTone /= bass chord then 1 else 0
    noRoot = if lowestTone /= base chord then 1 else 0
    hasBarre = if barre diagram > 0 then 1 else 0
    tooLittleBarreNotes = if length (filter (== barre diagram) playingStrings) < 2 then 1 else 0
    minTone = minimum playingStrings
    -- minPos, maxPos, firstString and lastString determine the hand position (comfortability)
    minPos = minimum safeNonzeroStrings
    maxPos = maximum safeNonzeroStrings
    firstString = safeIndexOfFirstHeldString (dStrings diagram)
    lastString = (availableStrings - 1) - safeIndexOfFirstHeldString (reverse (dStrings diagram))
    numberOfFingers = countFingers (dStrings diagram) (barre diagram)
    numberOfMuted = countMuted (dStrings diagram)
    hasMuted = if numberOfMuted > 0 then 1 else 0
    hasTooManyMuted = if numberOfMuted >= (availableStrings `div` 2) then 1 else 0

-- ZPRACOVÁNÍ VSTUPU --

-- oddělovače položek v konfiguračím souboru
listSeparator :: Char
listSeparator = ' '

propertySeparator :: Char
propertySeparator = ','

itemSeparator :: Char
itemSeparator = '\n'

blockSeparator :: Char
blockSeparator = ';'

-- výchozí číslo oktávy
defaultOctave :: Int
defaultOctave = 4

-- rozdělí seznam na seznam seznamů podle oddělovačů (prázdné položky se zahodí)
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep xs
  | null ys = rest
  | otherwise = ys : rest
  where
    rest = splitOn sep (dropFirst zs)
    (ys, zs) = break (== sep) xs
    dropFirst [] = []
    dropFirst xs = tail xs

-- načte tón se suffixem
-- přijímá String, vrací číslo tónu (nebo Nothing) a suffix
parseNote :: String -> (Maybe Int, String)
parseNote note = (fmap (+ accidental) noteName, remainingString)
  where
    notes = [('C', 0), ('D', 2), ('E', 4), ('F', 5), ('G', 7), ('A', 9), ('B', 10), ('H', 11)]
    accidentals = [('#', 1), ('b', -1)]

    noteName = lookup (head note) notes
    rem = tail note
    accidentalChar = if null rem then ' ' else head rem
    hasAccidental = (not . null) rem && accidentalChar `elem` map fst accidentals
    accidental = if hasAccidental then fromJust (lookup accidentalChar accidentals) else 0
    remainingString = if hasAccidental then tail rem else rem

-- načte tón s číslem oktávy
-- určeno pro čtení konfigurace hudebních nástrojů, může skončit chybou
parseNoteWithOctave :: String -> Int
parseNoteWithOctave note = fromJust parsed + (octave * 12) -- fromJust may fail
  where
    (parsed, octaveString) = parseNote note
    octave = if null octaveString then defaultOctave else read octaveString :: Int

-- načte akord
-- přijímá seznam povolených typů (struktur) akordů a jméno akordu, vrací akord (nebo Nothing)
parseChord :: [(String, [Int])] -> String -> Maybe Chord
parseChord chordTypes chord
  | isNothing base || isNothing structure || (hasBass && isNothing bass) = Nothing
  | otherwise = Just $ Chord (fromJust base) bass (fromJust structure)
  where
    parts = splitOn '/' chord
    basePart = head parts
    hasBass = length parts > 1
    (base, chordType) = parseNote basePart
    bass = if hasBass then fst (parseNote (parts !! 1)) else Nothing
    chordTypeToLookup = if null chordType then "major" else chordType
    structure = lookup chordTypeToLookup chordTypes

-- načte hudební nástroj, vrátí dvojici (název nástroje, nástroj)
parseInstrument :: String -> (String, Instrument)
parseInstrument inputString = (head instrument, Instrument (head instrument) strings frets)
  where
    instrument = splitOn propertySeparator inputString
    strings = map parseNoteWithOctave (splitOn listSeparator (instrument !! 1))
    frets = read (instrument !! 2) :: Int

-- načte typ akordu, vrátí seznam dvojic (název typu akordu, struktura akordu)
-- vrací seznam, aby bylo možné v konfiguraci jeden akord pojmenovat více jmény
parseChordType :: String -> [(String, [Int])]
parseChordType inputString = [(name, structure) | name <- names]
  where
    chordType = splitOn propertySeparator inputString
    names = splitOn listSeparator $ head chordType
    structure = map read (splitOn listSeparator (chordType !! 1)) :: [Int]

-- TISK VÝSTUPU --

-- (nepoužívaný) horizontální výpis diagramu bez čísel prstů
showDiagramHorizontally :: Diagram -> String
showDiagramHorizontally diagram = unlines (map diagramLine (reverse (dStrings diagram))) ++ "\n"
  where
    farthestFinger = maximum $ catPlaying (dStrings diagram)
    width = max farthestFinger 8
    diagramLine Muted = "x " ++ concat [if i == barre diagram then "| " else "- " | i <- [1 .. width]]
    diagramLine (Playing p) =
      (if p == 0 then "o " else "  ")
        ++ concat
          [ if i == p
              then "0 "
              else
                if i == barre diagram
                  then "| "
                  else "- "
            | i <- [1 .. width]
          ]

-- převádí seznam stavů strun na seznam souřadnic popisující stisknuté struny
-- přijímá číslo první struny a seznam stavů, vrací seznam dvojic (pražec, struna)
toCoordinates :: Int -> [StringState] -> [(Int, Int)]
toCoordinates _ [] = []
toCoordinates stringNumber (stringState : xs)
  | stringState == Muted || stringState == Playing 0 = toCoordinates (stringNumber + 1) xs
  | otherwise =
      let (Playing playing) = stringState
       in (playing, stringNumber) : toCoordinates (stringNumber + 1) xs

-- generuje seznam čísel prstů, které lze umístit do diagramu
-- na vstupu přijímá diagram a seznam dvojic (pražec, struna)
fingersToAssign :: Diagram -> [(Int, Int)] -> [Int]
fingersToAssign diagram coords
  | usedFingers == 1 = [min (fret 0) 3]
  | usedFingers == 2 = [1, min (2 + min (fret 1 `fs` fret 0) 2) maxFingersCount]
  | usedFingers == 3 = [1, finger2At3, min (finger2At3 + 1 + min (fret 2 `fs` fret 1) 1) maxFingersCount]
  | otherwise = [1 .. maxFingersCount]
  where
    usedFingers = countFingers (dStrings diagram) (barre diagram)
    fret i = fst $ coordsOnlyOneBarre !! i
    fs a b = max (a - b - 1) 0
    finger2At3 = if usedFingers > 1 then min (2 + min (fret 1 `fs` fret 0) 1) (maxFingersCount - 1) else 0
    coordsOnlyOneBarre = coordsOnlyOneBarre' coords False

    -- preserve only one barre coordinate in the list of all coordinates
    coordsOnlyOneBarre' [] _ = []
    coordsOnlyOneBarre' (x : xs) barrePresent
      | fst x /= barre diagram = x : coordsOnlyOneBarre' xs barrePresent
      | barrePresent = coordsOnlyOneBarre' xs True
      | otherwise = x : coordsOnlyOneBarre' xs True

-- přiřadí prstům čísla
-- na vstupu diagram a seznam dvojic (pražec, struna), na výstupu seznam "prstů"
assignFingers :: Diagram -> [(Int, Int)] -> [Finger]
assignFingers diagram coords = assignedFingers coords fingersToAssignWithoutBarre
  where
    f = fingersToAssign diagram coords
    fingersToAssignWithoutBarre = if barre diagram == 0 then f else tail f
    assignedFingers [] _ = []
    assignedFingers (x : xs) ys
      | fst x == barre diagram = uncurry Finger x 1 : assignedFingers xs ys
      | otherwise = uncurry Finger x (head ys) : assignedFingers xs (tail ys)

-- sloučí "prsty" do skupin podle pražců pro snazší tisk po pražcích
groupByFret :: [Finger] -> [(Int, [Finger])]
groupByFret [] = []
groupByFret (x : xs) = (fFret x, x : ys) : groupByFret zs
  where
    (ys, zs) = span (\y -> fFret y == fFret x) xs

-- z diagramu generuje seznam pražců (aby bylo možné jej snadno tisknout po řádcích)
-- přijímá diagram, vrací dvojici (číslo prvního pražce v seznamu, seznam pražců)
-- pražec v seznamu = seznam čísel prstů, Nothing = žádný prst (tedy pokud je na 2. pozici v seznamu Just 4, je třeba stisknout 2. strunu malíčkem)
fretFingerList :: Diagram -> (Int, [[Maybe Int]])
fretFingerList diagram = (firstFret, fingersOnFrets)
  where
    assignedFingers = assignFingers diagram $ sort (toCoordinates 1 (dStrings diagram))
    groupedFingers = groupByFret assignedFingers
    firstFret = if null groupedFingers then 1 else fst (head groupedFingers)
    numberOfStrings = length (dStrings diagram)
    fingersOnFrets = fretGroupsToFrets firstFret groupedFingers

    fretGroupsToFrets _ [] = []
    fretGroupsToFrets fret1 (x : xs)
      | fret1 < fret2 = replicate numberOfStrings Nothing : fretGroupsToFrets (fret1 + 1) (x : xs)
      | otherwise = map getFinger [1 .. numberOfStrings] : fretGroupsToFrets (fret1 + 1) xs
      where
        (fret2, fingers) = x
        strings = map (\z -> (fString z, fFinger z)) fingers
        getFinger string = string `lookup` strings

-- převede diagram na text
showDiagram :: Diagram -> String
showDiagram diagram = top ++ middle ++ bottom ++ "\n"
  where
    numberOfStrings = length (dStrings diagram)
    showFirstFretNumber = firstFret > 1
    fingerList = fretFingerList diagram
    fretCount = length (snd fingerList)
    firstFret = if (fst fingerList + fretCount) > 5 then fst fingerList else 1

    emptyLinesTop = fst fingerList - firstFret
    emptyLinesBottom = max (4 - emptyLinesTop - fretCount) 0
    emptyLine = "  " ++ concat (replicate numberOfStrings "| ")

    firstLine = "  " ++ concatMap stringToChar (dStrings diagram)
    stringToChar Muted = "x "
    stringToChar (Playing p) = if p == 0 then "o " else "  "

    top = unlines (firstLine : replicate emptyLinesTop emptyLine)
    bottom = unlines (replicate emptyLinesBottom emptyLine)
    middle = unlines (uncurry diagramLines fingerList (barre diagram) showFirstFretNumber)

    -- rekurzivně jednotlivé řádky diagramu (nalevo od prvního z nich v případě potřeby zobrazí číslo pražce)
    diagramLines _ [] _ _ = []
    diagramLines fretNumber (fret : xs) barre showFretNumber = (fn ++ fretString) : diagramLines (fretNumber + 1) xs barre False
      where
        barreString = concat (replicate (length fret * 2 - 1) "e")
        fretString = if barre == fretNumber then barreString else concatMap fingerToChar fret
        fn = if showFretNumber then show fretNumber ++ (if fretNumber < 10 then " " else "") else "  "
        fingerToChar Nothing = "| "
        fingerToChar (Just finger) = show finger ++ " "

-- HLAVNÍ FUNKCE --

-- nedeterministicky generuje všechny možné diagramy pro daný nástroj a akord
diagramsNoFilter :: Instrument -> Chord -> [Diagram]
diagramsNoFilter instrument chord = fillAllStrings instrument chord (generateBarres (frets instrument))

-- generuje povolené diagramy pro nástroj a akord
diagramsFiltered :: Instrument -> Chord -> [Diagram]
diagramsFiltered instrument chord = filter (checkDiagram instrument chord) (diagramsNoFilter instrument chord)

-- vrátí 10 nejlepších diagramů pro nástroj a akord
bestDiagrams :: Instrument -> Chord -> [(Int, Diagram)]
bestDiagrams instrument chord = take 10 $ sortBy (\x y -> compare (fst x) (fst y)) $ map (scoreDiagram instrument chord) (diagramsFiltered instrument chord)

-- vrátí textovou reprezentaci 10 nejlepších diagramů pro daný nástroj a akord
showBest :: Instrument -> Chord -> String
showBest instrument chord = concatMap (showDiagram . snd) (reverse (bestDiagrams instrument chord))

-- vstupně-výstupní funkce
main :: IO ()
main = do
  putStrLn "\nVítejte v aplikaci zobrazující kytarové (a jiné) hmaty.\n"
  putStrLn "Nyní se načte konfigurační soubor."
  args <- getArgs
  config <- readFile (if null args then "config.txt" else head args)
  putStrLn "Konfigurační soubor byl úspěšně načten.\n"

  let (instrumentsConfig : chordTypesConfig : _) = splitOn blockSeparator config
      instrumentsLines = splitOn itemSeparator instrumentsConfig
      chordTypesLines = splitOn itemSeparator chordTypesConfig
      instruments = map parseInstrument instrumentsLines
      chordTypes = concatMap parseChordType chordTypesLines

      loopInstrument = do
        putStr "Vyberte hudební nástroj: "
        hFlush stdout -- flushes stdout buffer
        instrumentString <- getLine
        let maybeInstrument = lookup instrumentString instruments
        maybe loopInstrument return maybeInstrument

      loopChord instrument = do
        putStr "Zadejte akord: "
        hFlush stdout -- flushes stdout buffer
        chordString <- getLine
        let chord = parseChord chordTypes chordString
            info = "akord " ++ chordString ++ ", " ++ iName instrument ++ "\n"
            bestChords = maybe "" (\c -> showBest instrument c ++ info) chord
        putStr bestChords
        when (chordString /= "konec") (loopChord instrument)

  putStrLn "Nejprve zvolte nástroj, poté zadejte název akordu. Nejlepší hmaty se zobrazí nejníže."
  putStrLn "V režimu zadávání akordů můžete aplikaci ukončit, napíšete-li \"konec\"."
  putStrLn ("Podporované nástroje:" ++ tail (concatMap (\i -> ", " ++ fst i) instruments))
  instrument <- loopInstrument
  loopChord instrument

-- testy
tests :: [Bool]
tests =
  [ -- kytara E
    dataEDiagram `elem` map snd (bestDiagrams dataGuitar dataEChord),
    -- kytara C
    Diagram {dStrings = [Muted, Playing 3, Playing 2, Playing 0, Playing 1, Playing 3], barre = 0} `elem` map snd (bestDiagrams dataGuitar dataCChord),
    -- ukulele C
    Diagram {dStrings = [Playing 0, Playing 0, Playing 0, Playing 3], barre = 0} `elem` map snd (bestDiagrams dataUkulele dataCChord),
    -- ukulele E (barré)
    Diagram {dStrings = [Playing 4, Playing 4, Playing 4, Playing 7], barre = 4} `elem` map snd (bestDiagrams dataUkulele dataEChord),
    -- ukulele E (bez barré)
    Diagram {dStrings = [Playing 1, Playing 4, Playing 0, Playing 2], barre = 0} `elem` map snd (bestDiagrams dataUkulele dataEChord),
    -- kytara C/E
    Diagram {dStrings = [Playing 0, Playing 3, Playing 2, Playing 0, Playing 1, Playing 3], barre = 0}
      `elem` map snd (bestDiagrams dataGuitar Chord {base = 0, bass = Just 4, structure = [0, 4, 7]}),
    -- převod na souřadnice
    toCoordinates 1 (dStrings dataEDiagram) == [(2, 2), (2, 3), (1, 4)],
    -- převod na seznam pražců
    fretFingerList dataEDiagram == (1, [[Nothing, Nothing, Nothing, Just 1, Nothing, Nothing], [Nothing, Just 2, Just 3, Nothing, Nothing, Nothing]]),
    -- kontrola diagramu (správný diagram)
    checkDiagram dataGuitar dataEChord dataEDiagram,
    -- kontrola diagramu (špatný diagram)
    not $ checkDiagram dataGuitar dataCChord dataEDiagram,
    -- správné ohodnocení kytarového akordu podle nejnižšího znějícího tónu
    fst (scoreDiagram dataGuitar dataCChord (Diagram {dStrings = [Muted, Playing 3, Playing 2, Playing 0, Playing 1, Playing 3], barre = 0}))
      < fst (scoreDiagram dataGuitar dataCChord (Diagram {dStrings = [Playing 0, Playing 3, Playing 2, Playing 0, Playing 1, Playing 3], barre = 0})),
    -- dělení seznamu
    splitOn ',' ",,,ab,c,d,,,,efg,,," == ["ab", "c", "d", "efg"],
    -- zpracování not
    parseNote "Cmaj" == (Just 0, "maj"),
    parseNote "E#mi" == (Just 5, "mi"),
    parseNote "Ebmi" == (Just 3, "mi")
  ]
