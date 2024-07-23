# Generátor hmatů akordů

| Vít Kološ, 2. ročník, IPP | letní semestr 2023/2024 | NPRG005 Neprocedurální programování |
| - | - | - |

## Anotace

Program pro zadaný akord určí hmaty, kterými jej lze na kytaru či jiný strunný nástroj zahrát, a zobrazí jejich diagramy.

## Kompletní specifikace

Program generuje kytarové hmaty. Uživatel zadá název akordu (třeba Cmaj7 nebo Gadd2) a program vygeneruje všechny způsoby, jak lze akord (rozumně) zahrát. Ty uspořádá podle použitelnosti – tedy například podle toho, jak jsou prsty na hmatníku daleko od sebe nebo zda je v akordu použito barré.

Na vstup (nebo spíše v konfiguračním souboru) program dostane jednotlivé typy akordů (aby bylo jasné, co znamená maj7) a informace o strunách. Vstup a výstup je textový. Kromě základních akordů program podporuje i ty s basovými tóny (např. C/E). Zobrazuje očíslování prstů, aby uživatel věděl, kterou strunu má držet ukazováčkem a kterou raději malíčkem.

Tento projekt volně navazuje na můj zápočtový program z Programování 2, v němž jsem stejnou úlohu řešil v C#.

## Použití programu

Za předpokladu, že je program správně konfigurován (viz sekce [Konfigurace](#konfigurace)), spočívá jeho použití ve zvolení nástroje (zadáním jeho názvu) a dále zadání [názvu akordu](#název-akordu) (např. D#maj7). Následně se zobrazí několik způsobů, jak daný akord na konkrétní nástroj zahrát. Tyto varianty jsou seřazeny podle jistých kritérií (z praktických důvodů ve vzestupném pořadí, takže nejlepší hmat je vypsán jako poslední), obvykle je vhodné použít jeden z posledních tří zobrazených hmatů. Poté lze zadat další název akordu nebo program ukončit pomocí klíčového slova „konec“.

### Název akordu

Název akordu se může skládat ze tří částí: základního tónu, typu akordu a basového tónu (za lomítkem). Všechny podporované typy akordů musí být uvedeny v konfiguraci. Není-li typ akordu zadán, je automaticky vyhodnocen jako `major` (tento typ akordu by tedy v konfiguraci neměl chybět). Basový tón se píše až za typ akordu a musí být zleva oddělen lomítkem.

Tóny (základní i basové) jsou tvořeny jedním nebo dvěma znaky, kde první je velké písmeno a druhý je znak `#` nebo `b`. Písmena odpovídají českému (respektive německému) systému pojmenování not, tedy po notě A následují (po půltónech) B, H, C. (V anglosaském systému by to bylo A, Bb, B, C.) Funguje enharmonická záměna, tedy platí, že G# = Ab nebo že B = Hb = A#. Dvojité posuvky (Gbb) ani alternativní označení (Dis, Es) nejsou podporovány.

Příklady možných názvů akordů: `C/E`, `Ebmaj7`, `Fsus4`, `F#m`

## Konfigurace

Program očekává existenci konfiguračního souboru `config.txt` v aktuálním adresáři. Ten se načítá při startu programu, pokud tedy program běží a tento soubor je změněn, je nutné program spustit znova.

Adresu konfiguračního souboru lze také zadat jako argument příkazové řádky při spuštění programu (v takovém případě se soubor bude hledat na dané adrese).

Konfigurační soubor se skládá ze dvou částí oddělených středníkem. První část odpovídá seznamu hudebních nástrojů, druhá seznamu typů akordů. Jeden řádek odpovídá jednomu nástroji nebo typu akordu. Každý řádek obsahuje dvě nebo tři hodnoty oddělené čárkou.

### Konfigurace hudebního nástroje

U hudebního nástroje určuje první hodnota jeho název (nesmí obsahovat čárku ani středník).

Druhá určuje ladění strun, jde o seznam tónů odpovídajících jednotlivým strunám, pořadí v seznamu odpovídá pořadí na nástroji (zleva doprava). Tóny je nutné oddělit mezerami, za název tónu se uvádí číslo oktávy (podle *vědecké* notace, kde jednočárkovaná oktáva má číslo 4, *komorní a* by se tedy zapsalo jako A4). Pokud číslo oktávy chybí, je použita výchozí (jednočárkovaná) oktáva. Specifika zápisu tónu jsou uvedeny v sekci [Název akordu](#název-akordu).

Poslední hodnota odpovídá počtu dostupných pražců (nultý se nepočítá). I u nástrojů s delším hmatníkem je lepší volit nižší hodnoty (typicky mezi 10 a 20), jelikož vyšší vedou k výrazně delším časům výpočtu a většina základních (výhodných) hmatů se vejde na prvních několik pražců.

### Konfigurace typu akordu

První hodnota odpovídá seznamu možných označení akordu oddělených mezerou. Speciální roli hraje označení `major`, neboť pokud uživatel při používání programu nezadá typ akordu, automaticky se zvolí tento typ. Označení by nemělo začínat znaky `#` nebo `b`, nesmí obsahovat čárku, středník ani mezeru.

Druhá hodnota obsahuje seznam tónů, z nichž se akord skládá, tóny jsou zde reprezentovány svými vzdálenostmi od tóniky. Každý tón tedy odpovídá číslu z rozsahu 0–11. Jednotlivé tóny se v seznamu oddělují mezerami.

### Vzorová konfigurace

```text
kytara,E2 A2 D3 G3 H3 E4,10
ukulele,G C E A,19
;
major,0 4 7
m mi,0 3 7
aug,0 4 8
7,0 4 7 10
m7,0 3 7 10
maj maj7,0 4 7 11
```

## Řešený problém a algoritmus

Jelikož jsem problém generování kytarových hmatů řešil již vloni v C#, tedy v procedurálním jazyce, nejzásadnější výzvou nebylo vymyslet algoritmus, kterým by bylo možné problém řešit, ale spíše ověřit, zda lze ve funkcionálním programování použít přístup podobný tomu, jež jsem dříve úspěšně implementoval imperativně.

Průchod programem lze v zásadě rozdělit do šesti fází: 1. načtení vstupu, 2. generování nějakých hmatů, 3. filtrování těch přijatelných, 4. ohodnocení hmatů a jejich seřazení, 5. očíslování prstů, 6. výpis. Za ty nejzajímavější považuji fáze filtrování, ohodnocování a číslování prstů.

### Vhodný hmat

Klíčovou otázkou, jejíž zodpovězení vyžaduje jistou osobní zkušenost s hrou na strunný hudební nástroj či alespoň rámcovou znalost této domény, je způsob filtrování a ohodnocení jednotlivých hmatů. Od hmatu budeme obvykle požadovat, aby se jednak dobře hrál, ale také aby dobře zněl. To zní poměrně subjektivně, ale zcela jistě lze definovat několik objektivních podmínek, které každý platný hmat musí splňovat:

1. musí obsahovat všechny tóny akordu a žádné jiné,
2. k jeho stisknutí jsou použity nejvýše čtyři prsty,
3. je-li ve hmatu použito barré, je aktivně využíváno,
4. obsahuje-li hmat tlumené struny, jsou pouze na jednom kraji hmatníku.

Pro ukulele (čtyřstrunný nástroj s 19 pražci) teoreticky existuje 160 000 hmatů. Po odstranění akordů, které nesplňují tato kritéria, jich zbyde přibližně 700. Ty je potřeba nějak seřadit a určit, které jsou nejvýhodnější.

Klíč, podle nějž lze hmaty řadit, zajišťuje ohodnocovací funkce, která každému hmatu přiděluje skóre (respektive penalizaci). To je ovlivněno těmito kritérii: barré, základní tón (zda je nejnižším tónem souzvuku), výška nejnižšího tónu, výškové rozpětí tónů v souzvuku, vzájemná vzdálenost prstů na hmatníku, počet použitých prstů, přítomnost tlumených strun. Je tam rovněž několik kritérií, které mají tak výraznou váhu, že akord obvykle odsunou z první desítky: téměř nevyužívané barré, basový tón není nejnižším tónem, příliš mnoho tlumených strun. Některá kritéria mají dokonce různou váhu v závislosti na počtu strun daného hudebního nástroje.

Ohodnocovací funkci jsem vytvářel ručně, snažil jsem se, aby tradiční hmaty všech základních akordů byly na prvním místě nebo alespoň v první trojici. Sice stále nevrací dokonalé výsledky, ale někdy se ukazuje, že „správný“ výsledek ani vrátit nemůže, jako v případě kytarového akordu C7, jehož tradiční hmat neobsahuje tón g.

### Prsty a diagramy

Procedurální číslování prstů bylo poměrně jednoduché, vymyslet funkcionální implementaci pro mě byl trochu oříšek. Nakonec jsem to vyřešil rozborem případů podle počtu použitých prstů. Základ spočívá v číslování shora dolů a zleva doprava, ale pokud hmat nepoužívá všechny čtyři prsty, občas se nějaký prst *přeskočí*. Tady se projevila neohrabanost kódování hmatu jako seznamu strun. Rozhodl jsem se proto přidat několik převodních funkcí, s jejichž pomocí lze dostat hmat do formátu, kdy je reprezentovaný po pražcích. Tato podoba zároveň umožňuje snazší výpis akordu v tradiční svislé orientaci.

## Vývojová dokumentace

Kód jsem rozdělil do devíti sekcí: typy, testovací data, pomocné funkce, generování, kontroly, skóre, zpracování vstupu, tisk výstupu a hlavní funkce. Některé tyto části přímo odpovídají jednotlivým fázím průchodu programem.

### Typy

Slouží ke snazší reprezentaci složitějších entit. Všechny (automaticky) odvozují typové třídy `Show` a `Eq`.

#### Hudební nástroj (Instrument)

- `iName :: String` – název nástroje
- `iStrings :: [Int]` – seznam ladění jednotlivých strun v pořadí zleva; např. ukulele má ladění GCEA, což lze zapsat jako `[7, 0, 4, 9]` nebo správněji `[55, 48, 52, 57]` (v jednočárkované oktávě)
- `frets :: Int` – horní mez (inkluzivní) pro čísla pražců, které lze v akordech použít; má smysl používat čísla mezi 10 a 20, použití nižšího může negativně ovlivnit kvalitu výsledných akordů, volba vyššího čísla naopak výrazně zpomaluje algoritmus

#### Akord (Chord)

- `base :: Int` – základní tón akordu (na škále od 0 do 11)
- `bass :: Maybe Int` – basový tón akordu (absolutní, od 0 do 11), pokud jde o akord bez basového tónu, používá se `Nothing`
- `structure :: Int` – typ/struktura akordu, jde o seznam intervalů, které akord obsahuje (jsou to relativní vzdálenosti od základního tónu, na škále od 0 do 11)

#### Hmat (Diagram)

- `dStrings :: [StringState]` – seznam stavů strun, v pořadí zleva doprava popisuje, zda je struna tlumená, zda je prázdná nebo kde by ji měl hráč stisknout
- `barre :: Int` – číslo pražce s barré; pokud hmat nemá barré, je rovno nule (jelikož nultý pražec plní roli „barré“)

#### Stav struny (StringState)

Slouží k reprezentaci stavů strun v rámci hmatu. Může nabývat těchto hodnot:

- `Muted`, pokud struna nehraje
- `Playing 0`, pokud je struna prázdná
- `Playing N` pro N od nuly do maximálního povoleného pražce (včetně), pokud má hráč strunu stisknout na N-tém pražci

#### Prst (Finger)

Pomocný typ sloužící k nakládání s hmaty ve formátu seznamu „prstů“ (tedy konkrétních pozic, kam má hráč umístit prsty, a čísel jeho prstů).

- `fFret :: Int` – číslo pražce, kam se má prst umístit
- `fString :: Int` – číslo struny, kam se má prst umístit
- `fFinger :: Int` – číslo prstu, který se má použít

### Testovací data

Sada konstant umožňující jednodušší (ruční i automatizované) testování některých čistých funkcí.

### Pomocné funkce

Základní převody pro práci s akordy a hmaty. Mnohé se týkají konverze (relativních) intervalů na (absolutní) tóny a naopak.

### Generování

Tři funkce *nedeterministicky* generující všechny hmaty, které mají šanci být přípustné. Nejprve se umístí barré postupně na každý pražec (včetně nultého, tedy hmatu bez barré), následně se struna zkouší stisknout všemi možnými způsoby tak, aby hrál nějaký tón z daného akordu.

### Kontroly

Slouží k pročištění nedeterministicky vygenerovaných hmatů, aby zůstaly jen ty přípustné. Aplikují se čtyři pravidla [popsaná výše](#vhodný-hmat) – tóny akordu, počet prstů, použití barré, tlumené struny.

### Skóre

Skórovací funkce přiřazuje hmatu penalizaci. Na té se podílí několik složek rovněž [uvedených výše](#vhodný-hmat), mj. vzdálenost prstů od sebe nebo také otázka, zda nejnižší znějící tón je ten správný (basový tón, případně tónika).

### Zpracování vstupu

Několik funkcí pro načtění konfigurace a zpracování uživatelského vstupu z konzole.

Ty, které zpracovávají vstup z konzole jsou implementovány *bezpečně*, aby uživateli nehrozilo, že při zadání neplatného názvu akordu program spadne. Pro jednoduchost předpokládám korektnost konfiguračního souboru.

Funkce `parseNote` je univerzální – používá se při zpracování konfigurace i uživatelského vstupu. Načte vždy první znak a pokouší se jej interpretovat jako název tónu (pokud neuspěje, vrátí `Nothing` jako první prvek dvojice). Načítá rovněž druhý znak, očekává posuvku (pokud to není křížek ani béčko, považuje jej za součást následujícího textu). Zbytek zpracovávaného textu pak vrátí jako druhou položku dvojice.

### Tisk výstupu

Tato sekce obsahuje dva algoritmy pro tisk diagramů hmatů – jednoduchý pro horizontální výpis diagramu bez čísel prstů a složitější pro vertikální výpis s čísly prstů. Rovněž obsahuje dvě funkce zajišťující číslování prstů a tři funkce pro převod ze *strunové* reprezentace hmatu (pomocí `StringState`) na tu *pražcovou* (velmi podobnou textové reprezentaci vertikálního diagramu).

Proces vertikálního tisku diagramu probíhá přibližně takto:

1. diagram se převede na souřadnice pozic na hmatníku, na nichž má hráč mít položené prsty
2. na základě těchto souřadnic se vygeneruje seznam čísel použitých prstů
3. seznam se spáruje s pozicemi, takže výsledkem je seznam *prstů* (Finger)
4. seznam *prstů* se převede na *pražcovou* reprezentaci hmatu
5. hmat se vypíše

### Hlavní funkce

Jde o *zastřešující* funkce pro generování hmatů (vhodně kombinují funkce z ostatních sekcí), dále vstupně-výstupní funkci `main` a sadu základních testů.

#### Průchod programem (funkce main)

Funkce `main`, zajišťující komunikaci uživatele s programem, je realizována pomocí několika `do` bloků. Sestává z těchto kroků:

1. načtení konfigurace ze souboru na adrese uvedené jako argument příkazové řádky (pokud chybí, použije se soubor `config.txt` v aktuálním adresáři)
1. zpracování konfigurace
1. informační výpisy pro uživatele
1. výpis podporovaných hudebních nástrojů
1. volba nástroje (pokud uživatel zadá neexistující název nástroje, může zadání opakovat)
1. volba akordu
1. výpis vhodných hmatů (pokud uživatel zadal platný název akordu)
1. konec, pokud uživatel místo názvu akordu napsal „konec“, jinak lze zadat další akord

#### Testy

Připravil jsem sadu několika základních testů. Jsou realizovány pomocí funkce `tests`, která vrací seznam booleovských hodnot – úspěšnému testu odpovídá `True`, neúspěšnému `False`. Testuje se přítomnost některých známých (tradičních) hmatů mezi top 10 pro dané akordy. Dále převod hmatu do podoby souřadnic a převod na seznam pražců. Rovněž se kontroluje, zda filtrovací funkce správně označuje hmaty za přípustné, respektive nepřípustné a zda ohodnocovací funkce dva konkrétní hmaty správně seřadila. Nakonec jsem zahrnul pár testů na zpracování vstupu.
