Supplementary Information: Learning Salient Morphological Variation
================
Rácz, Péter
2023-06-14

# Structure

There were three experiments. All experiments were coded in javascript
and run remotely on the participant’s home computer. The workhorse of
the experiments was the written binary forced choice trial, in which you
see a written prompt and you have to finish it using one of two options.

The `baseline` experiment was used to benchmark the stimuli with one
sample of participants, so that we know that the participant pool
prefers variant 1 for word x but variant 2 for word y.

The `matching` experiment (referenced as `esp`, after “extra-sensory
perception”, in the file structure) was run with a second set of
participants. Its first half was a pretend game with a pretend second
player. The two of you have to pick the same ending for each prompt.
Since the second player is inflexible, you only succeed in the game if
you anticipate their responses. Its second half was similar to the
baseline experiment in that you have to pick options to finish prompts
by yourself. This is to check whether the patterns you learned in the
first half persist once the second player disappears.

The `integration` experiment was run with a third set of participants.
Its first part is the `matching` experiment, with the matching game and
the immediate post-test. It also involved a second post-test, after a
period of sleep.

The main questions of the paradigm are: Can people learn the
morphological variations fed to them in the matching game? Does this
knowledge persist and influence their responses in the immediate post
test? Does it persist and influence responses in the second post test,
after considerable time has passed?

# Stimuli

There are three sets of stimuli, variable patterns from Hungarian
morphology. `Baseline` participants see examples from all three.
`Matching game` participants only see one set of stimuli throughout.

## Sets of variation

We call the first variation `levelling`. It involves verbal inflection.
Hungarian inflects verbs with definite and indefinite objects
differently. The `1sg.pres.indef` of some verbs varies between the `-k`
inflectional suffix (which is the default for this exponent) and the
`-m` suffix (which is otherwise the `1sg.pres.def` suffix) – see
examples 1-4. Levelling is confined to one verb class (verbs that end in
`-ik` in the canonical form, the `3sg.pres.indef`) and one paradigm
slot, not appearing outside the `1sg.pres.indef`.

1)  **Nézem az almát.** `watch.1sg.pres.def the apple.acc` I watch the
    apple.

2)  **Nézek egy almát.** `watch.1sg.pres.indef an apple.acc` I watch an
    apple.

3)  **Eszem az almát.** `eat.1sg.pres.def an apple.acc` I eat the apple.

4)  **Eszek/eszem egy almát.** `eat.1sg.pres.indef an apple.acc` I eat
    an apple.

Many frequent verbs and many derivational suffixes are in the `-ik`
category, meaning that levelling is productive and applies to a large
number of frequent forms. It shows structured variation, sensitive to
derivational morphology, frequency, and form-based similarity. It has no
clear phonotactic or morphophonological triggers, any verb can end in
`-k` or `-m` (as they do otherwise in the `indef` and the `def`,
respectively). It is also has second-order indexicality: the
neutralising `-m` variant is seen as educated, “proper” use ([Rácz
2019](https://www.jstor.org/stable/26863550)).

A turn-of-the-century English analogy would be variation between `will`
and `shall` in the first person, where `shall` was seen as more formal
and educated but was also more likely to stand with certain more common
verbs rather than others: compare “We shall see.” with “I shall bicycle
home.”.

The second variation is called `vowel deletion`. It also involves verbal
inflection and is also confined to `-ik` verbs. These verbs end in two
possible templates: CCik and CVCik. In certain cases, the vowel is
deleted: Some verbs have a fixed CC template, others a fixed CVC
template. Some verbs vary between CC and CVC across different paradigm
slots (examples 5-6). Some verbs vary between CC and CVC in the same
paradigm slot (example 7). This can be different paradigm slots for
different verbs. Some instances of vowel deletion bring with themselves
a change in the second consonant (example 7). Some cases of variation
are lexicalised (example 8) ([Rácz, Rebrus, & Törkenczy
2018](https://www.degruyter.com/document/doi/10.1515/cllt-2018-0014/html)).

5)  **Felsöpör.** `perf.sweep.3sg.pres.indef` (S)he sweeps up.

6)  **Felsöpri.** `perf.sweep.3sg.pres.def` (S)he sweeps it up.

7)  **Nem tudják, mit cselekszenek/cselekednek.**
    `neg know.2pl.pres.def what do.2pl.pres.indef` They know not what
    they do.

8)  **Ma az áldozatokra emlékezünk/emlékszünk.**
    `today the victim.pl.loc remember/commemorate.2pl.pres.indef` Today
    we remember/honour the victims.

Variation is partly phonotactically and partly morphophonologically
motivated. Certain pairs of consonants cannot occur adjacently in the
stem unless they can be resyllabified, and this interacts with
restrictions on the linking vowel in the suffix. In some verbs, this
interaction leads to variable vowel deletion. In others, it cannot be
resolved and results in morphological defectivity ([Rebrus & Törkenczy
2009](https://www.researchgate.net/profile/Miklos-Toerkenczy/publication/299131111_Covert_and_overt_defectiveness_in_paradigms/links/56eee47608ae59dd41c7082b/Covert-and-overt-defectiveness-in-paradigms.pdf)).
While subject to some metalinguistic awareness, vowel deletion does not
carry any clear second-order indices and has not been discussed as a
sociolinguistic marker in Hungarian.

Vowel deletion in rapid speech can be observed in spoken English:
“family” or “natural” can be pronounced either as \[’f6mIlI\] or
\[’f6mlI\] and \[’n6t@r@l\] or \[’n6tr@l\]. This, however, is a
lower-level phenomenon, gradient, closely related to vowel stress, not
lexicalised, and not represented in the orthography.

The third variation is called `front harmony`. Hungarian has vowel
harmony: in most cases, the vowel in a suffix has to match the last
vowel in the stem in both frontedness and roundness (examples 9-10).
Some stem vowels are neutral, meaning that they do not determine the
suffix vowel: Vowel harmony “skips” them and matches the suffix vowel to
the preceding vowel in the stem (example 11). The vowel `e` is variably
neutral: a stem with a back vowel and `e` can select a front suffix
(matching `e`) or a back suffix (skipping `e` and matching the preceding
back vowel) (example 12).

9)  **fójerben** `foyer.loc` In the foyer.

10) **garázsnak** `garage.dat` To the garage.

11) **partival** `party.ins` With the party.

12) **hotelra/hotelre** `hotel.loc` On the hotel.

Hungarian vowel harmony and its triggers of variation have seen
extensive scrutiny in the theoretical linguistics literature (see
e.g. (Hayes, Siptár, Zuraw, & Londe
2009)\[<https://www.jstor.org/stable/40492955>\]).

## Experimental stimuli

We compiled a frequency list from the morphologically disambiguated
(Hungarian Webcorpus 2)\[<https://hlt.bme.hu/en/resources/webcorpus2>\].
This was used to curate reference lists of existing forms for the three
variable sets
(scripts/build_words/build_real_list/build_real_word_lists/R). (In the
scripts, levelling is called `ik`, vowel deletion is called `ep`, and
front harmony is called `vh`.) The reference lists were generated by
creating variable forms by hand, and cross-referencing them with the
frequency list and a (spelling
dictionary)\[<https://github.com/laszlonemeth/magyarispell>\].

The next step was to use the reference sets to generate nonce forms for
each variable sets. Because both vowel deletion and front harmony
involve longer dependencies in the CV skeleton, off-the-shelf ngram
models were not sufficient to generate nonce forms. Instead, we wrote a
simple grammar to break up the words on the reference list to
syllable-based units and generate noun and verb forms and checked the
results by hand. The resulting lists were filtered so that each nonce
word in each set (levelling, vowel deletion, front harmony) had at an
edit distance of 2 or more with words in the reference set and with each
other (scripts/build_words/build_nonce/list/\*). Edit distance was based
on a phonemic transcription of contrastive segments (as the language has
digraphs in the orthography).

The final stimuli list was further restricted to vary across set
dimensions. For levelling, the paradigm slot was set: this variation is
restricted to `1sg.indef`. Each nonce form had a one or bisyllabic stem
and a derivational suffix. Hungarian verbs are a closed class, and new
verbs enter the language via derivational morphology, using one of
several possible derivational suffixes (see examples 13-15).

13) **guglizik** `google.v.der.3sg.pres.indef` (S)he googles.

14) **dúmozik** `doom.v.der.3sg.pres.indef` (S)he plays the video game
    Doom..v.der.3sg.pres.indef

15) **solymászik** `falcon.v.der.3sg.pres.indef` (S)he falcons.

A set of examples for levelling can be seen in Table 1.

| base      | variant1  | variant2  | derivational | nsyl | infl |
|:----------|:----------|:----------|:-------------|-----:|:-----|
| floglik   | floglok   | floglom   | -lik         |    1 | 1sg  |
| klivaslik | klivaslok | klivaslom | -lik         |    2 | 1sg  |
| bűtszik   | bűtszök   | bűtszöm   | -szik        |    1 | 1sg  |
| bivégszik | bivégszek | bivégszem | -szik        |    2 | 1sg  |
| spírzik   | spírzok   | spírzom   | -zik         |    1 | 1sg  |
| plövőrzik | plövőrzök | plövőrzöm | -zik         |    2 | 1sg  |

Levelling: example stimuli

For vowel deletion, which occurs across many paradigm slots, variation
was restricted to the first, second, or third person of the plural
present indefinite. Otherwise, the structure was similar to levelling,
with mono- or bisyllabic stems and the same three derivational suffixes.
A set of examples for vowel deletion can be seen in Table 2.

| base                    | variant1     | variant2    | derivational | nsyl | infl |
|:------------------------|:-------------|:------------|:-------------|-----:|:-----|
| kléglik / klégezik      | kléglünk     | klégezünk   | -lik         |    1 | 1pl  |
| lűklik / lűközik        | lűklenek     | lűköznek    | -lik         |    1 | 2pl  |
| brüszlik / brüszözik    | brüszlötök   | brüszöztök  | -lik         |    1 | 3pl  |
| frivamlik / frivamozik  | frivamlunk   | frivamozunk | -lik         |    2 | 1pl  |
| trálaklik / trálakozik  | trálaklanak  | trálakoznak | -lik         |    2 | 2pl  |
| flüröklik / flüröközik  | flüröklötök  | flürököztök | -lik         |    2 | 3pl  |
| trálszik / trálodik     | trálszunk    | trálodunk   | -szik        |    1 | 1pl  |
| fökszik / föködik       | fökszenek    | föködnek    | -szik        |    1 | 2pl  |
| molszik / molodik       | molszotok    | molodtok    | -szik        |    1 | 3pl  |
| vehékszik / vehékedik   | vehékszünk   | vehékedünk  | -szik        |    2 | 1pl  |
| privékszik / privékedik | privékszenek | privékednek | -szik        |    2 | 2pl  |
| frálapszik / frálapodik | frálapszotok | frálapodtok | -szik        |    2 | 3pl  |
| csubzik / csubozik      | csubzunk     | csubozunk   | -zik         |    1 | 1pl  |
| trugzik / trugozik      | trugzanak    | trugoznak   | -zik         |    1 | 2pl  |
| sprigzik / sprigozik    | sprigzotok   | sprigoztok  | -zik         |    1 | 3pl  |
| nyivágzik / nyivágozik  | nyivágzunk   | nyivágozunk | -zik         |    2 | 1pl  |
| bratárzik / bratározik  | bratárzanak  | bratároznak | -zik         |    2 | 2pl  |
| klarágzik / klarágozik  | klarágzotok  | klarágoztok | -zik         |    2 | 3pl  |

Vowel deletion: example stimuli

For front harmony, all stems were bisyllabic, with a back vowel and the
variable neutral vowel `e` or `é`. Many paradigm slots of the case
system show variation. Here, it was restricted to the inessive, the
dative, and the adessive case. A set of examples for front harmony can
be seen in Table 3.

| base     | variant1    | variant2    | vowel | decl |
|:---------|:------------|:------------|:------|:-----|
| dalenc   | dalencnál   | dalencnél   | e     | ad   |
| zihenc   | zihencnak   | zihencnek   | e     | dat  |
| szibed   | szibedban   | szibedben   | e     | in   |
| fágyép   | fágyépnál   | fágyépnél   | é     | ad   |
| klordény | klordénynak | klordénynek | é     | dat  |
| fájéz    | fájézban    | fájézben    | é     | in   |

Front harmony: example stimuli

Three sets of 162 forms each were created, balancing across the
restrictions above. For each nonce word in each variable set, there are
two possible variant choices: `-k` or `-m` for levelling, `cc` (vowel
deleted) or `cvc` (vowel in situ) for vowel deletion, and back vowel or
front vowel in suffix for front harmony.

# Baseline task

## Stimuli

Each set of 162 stimuli were split to three lists. Each participant was
randomly given one list for each variable set (levelling, vowel
deletion, front harmony), 162 stimuli in total, in random order, not in
blocks. Each stimulus was embedded in a prompt that the participant had
to complete using one of two choices. The prompt was fixed across
variable set and inflectional or declension suffix. The prompt ending
unequivocally selects for a specific verb/noun complement. The two
variants of this complement are specified as the two alternatives in the
forced choice task. Example prompts can be seen in Table 4.

| prompt                                                    | variant1    | variant2    | gloss                                                         | variable set   |
|:----------------------------------------------------------|:------------|:------------|:--------------------------------------------------------------|:---------------|
| Te bizony sokat spröbzöl. Én is sokat…                    | spröbzök    | spröbzöm    | You do frequently VERB. I also frequently…                    | levelling      |
| János imád galározni, ezért sokat galárzik. Ők ritkán…    | galárzanak  | galároznak  | John loves to CVC VERB so he frequently CC VERB. They rarely… | vowel deletion |
| János imád dilakozni, ezért sokat dilaklik. Ti ritkán…    | dilaklotok  | dilakoztok  | John loves to CVC VERB so he frequently CC VERB. You rarely…  | vowel deletion |
| János imád flerékedni, ezért sokat flerékszik. Mi ritkán… | flerékszünk | flerékedünk | John loves to CVC VERB so he frequently CC VERB. We rarely…   | vowel deletion |
| Ez itt egy trikép. Ott vagyunk a…                         | triképban   | triképben   | This is a NOUN. We are there in the…                          | front harmony  |
| Ez itt egy hágyét. Elneveztem a kutyámat…                 | hágyétnak   | hágyétnek   | This is a NOUN. I named my dog…                               | front harmony  |
| Ez itt egy vismer. Nincs is jobb egy jó…                  | vismernál   | vismernél   | This is a NOUN. Nothing better than a good…                   | front harmony  |

Example prompts across variation and suffix

## Participants

82 participants, 69 women, median age = 22 took part in the baseline
experiment in early 2022, participating for course credit.

## Procedure

The task was coded in
(psychojs)\[<https://www.sciencedirect.com/science/article/pii/S0165027006005772>\]
and hosted on (Pavlovia)\[pavlovia.org\]. Participants were instructed
that they would see prompts that contain words that might be unfamiliar
to them. They were asked to pick a suffixed form to finish the prompt in
a way that seems most natural or familiar to them. They were informed
that different people might have different preferences and there are no
correct or incorrect answers. An example preceded the trials.
Participants used the keyboard to respond.

## Hypotheses

The aim of the task was to benchmark our stimuli. There were no starting
hypotheses on the distributions.

## Analysis

Participants who only picked the left button or the right button were to
be excluded from the data. No participants met this pre-registered
exclusion criterion.

## Results

Participants completed the task in a median 15.0747867 minutes. Given
the randomisation, the number of responses per nonce word was not the
same. Participants gave 26–29 responses to each nonce form, with a
median of 27.

For each nonce form, we calculated the log odds of the counts of
participants who picked variant 1 and those who picked variant 2.

## Data and code

Code used to set up and process the experiment is in
`scripts/baseline_exp`. The code to run the experiment is in
`interface/hesp_baseline.zip`. Raw data are in `exp_data/raw`, tidy data
are in `exp_data/baseline`.

## Ethics

Participants gave informed consent at the beginning of the experiment.
The study was approved by the United Ethical Review Committee for
Research in Psychology in Hungary (EPKEB, ref. number 2021-119).

# Matching task

The sample size, design, exclusion criteria, and data analysis were
[pre-registered](https://aspredicted.org/BL1_S7V). We only report the
levelling and vowel deletion data.

## Stimuli

The matching task is played with a co-player and the participant has to
match the way the co-player would finish the prompt. By manipulating the
co-player’s responses, we can feed different morphological distributions
to the participant. One participant sees words from one type of
variation only (levelling / vowel deletion / front harmony), both in the
matching task and the post test. One participant sees different words in
the matching task and in the post test.

We created three lists of 54 items each for each variation, sampled from
the baseline. In each list, nonce forms were ranked from 1 (largest
baseline preference for variant 1) to 54 (largest baseline preference
for variant 2). Co-players used the baseline patterns as a reference.
They diverged from them across two dimensions.

Co-players had a **high** rate of use for variant 1 (and,
correspondingly, a low rate of use for variant 2) or a **low** rate of
use for variant 1 (and a high rate for variant 2). **High** meant they
picked variant 1 in 39/54 trials (72%). **Low** meant they picked
variant 1 in 15/54 trials (28%).

Co-players didn’t pick forms at random. The lists were sorted according
to rank, going from words that showed the highest preference for variant
1 to those that showed the lowest preference for variant 1. One set of
co-players picked the first 39 forms (in the **high** condition) or the
first 15 forms (in the **low** condition) and then went on to choose
variant 1 for these. This meant that these co-players acted like our
average baseline participant, apart from having a quantitative
preference for lots of variant 1 or few of variant 1. These were
**typical** co-players.

Another set of co-players reversed the list and chose variant 1 for the
words that were least likely to select it in the baseline. Effectively,
these co-players gave responses that were the opposite of the
expectations of an average baseline participant. These were **reversed**
co-players.

Table 5 illustrates this. It shows the items on list 1 for the levelling
set, sorted from strongest preference for variant 1 (“tomlik”) to
strongest preference for variant 2 (“spilágzik”). A co-player with
**high** preference for variant 1 and a **typical** distribution will
pick the first variant, `-k` for the first 39 verbs and the second
variant, `-m`, for the last 15. A co-player with **low** preference for
variant 1 and a **typical** distribution will pick `-k` for the first 15
verbs and `-m` for the last 39. A co-player with **high** preference for
`-k` and a **reversed** distribution will use the list upside-down and
pick `-k` for the last 39 verbs and `-m` for the first 15 verbs. A
co-player with **low** preference for `-k` and a **reversed**
distribution will pick `-k` for the last 15 verbs and `-m` for the first
39 verbs.

| baseline rank | base       | high typical   | low typical    | high reversed  | low reversed   |
|--------------:|:-----------|:---------------|:---------------|:---------------|:---------------|
|             1 | tomlik     | tomlo**k**     | tomlo**k**     | tomlo**m**     | tomlo**m**     |
|             2 | fibamlik   | fibamlo**k**   | fibamlo**k**   | fibamlo**m**   | fibamlo**m**   |
|             3 | femlik     | femle**k**     | femle**k**     | femle**m**     | femle**m**     |
|             4 | feténylik  | fetényle**k**  | fetényle**k**  | fetényle**m**  | fetényle**m**  |
|             5 | spűmlik    | spűmlö**k**    | spűmlö**k**    | spűmlö**m**    | spűmlö**m**    |
|             6 | sztremlik  | sztremle**k**  | sztremle**k**  | sztremle**m**  | sztremle**m**  |
|             7 | pratánylik | pratánylo**k** | pratánylo**k** | pratánylo**m** | pratánylo**m** |
|             8 | spéslik    | spésle**k**    | spésle**k**    | spésle**m**    | spésle**m**    |
|             9 | flagánylik | flagánylo**k** | flagánylo**k** | flagánylo**m** | flagánylo**m** |
|            10 | jüslik     | jüslö**k**     | jüslö**k**     | jüslö**m**     | jüslö**m**     |
|            11 | büglik     | büglö**k**     | büglö**k**     | büglö**m**     | büglö**m**     |
|            12 | prűrlik    | prűrlö**k**    | prűrlö**k**    | prűrlö**m**    | prűrlö**m**    |
|            13 | ruslik     | ruslo**k**     | ruslo**k**     | ruslo**m**     | ruslo**m**     |
|            14 | marzik     | marzo**k**     | marzo**k**     | marzo**m**     | marzo**m**     |
|            15 | brüjlik    | brüjlö**k**    | brüjlö**k**    | brüjlö**m**    | brüjlö**m**    |
|            16 | csíszlik   | csíszlo**k**   | csíszlo**m**   | csíszlo**k**   | csíszlo**m**   |
|            17 | flüröslik  | flüröslö**k**  | flüröslö**m**  | flüröslö**k**  | flüröslö**m**  |
|            18 | szirlik    | szirlo**k**    | szirlo**m**    | szirlo**k**    | szirlo**m**    |
|            19 | klörlik    | klörlö**k**    | klörlö**m**    | klörlö**k**    | klörlö**m**    |
|            20 | nyáslik    | nyáslo**k**    | nyáslo**m**    | nyáslo**k**    | nyáslo**m**    |
|            21 | pebzik     | pebze**k**     | pebze**m**     | pebze**k**     | pebze**m**     |
|            22 | juszlik    | juszlo**k**    | juszlo**m**    | juszlo**k**    | juszlo**m**    |
|            23 | klibamlik  | klibamlo**k**  | klibamlo**m**  | klibamlo**k**  | klibamlo**m**  |
|            24 | tínylik    | tínylo**k**    | tínylo**m**    | tínylo**k**    | tínylo**m**    |
|            25 | bölszik    | bölszö**k**    | bölszö**m**    | bölszö**k**    | bölszö**m**    |
|            26 | flűröklik  | flűröklö**k**  | flűröklö**m**  | flűröklö**k**  | flűröklö**m**  |
|            27 | trilszik   | trilszo**k**   | trilszo**m**   | trilszo**k**   | trilszo**m**   |
|            28 | böjzik     | böjzö**k**     | böjzö**m**     | böjzö**k**     | böjzö**m**     |
|            29 | sűzlik     | sűzlö**k**     | sűzlö**m**     | sűzlö**k**     | sűzlö**m**     |
|            30 | hilarzik   | hilarzo**k**   | hilarzo**m**   | hilarzo**k**   | hilarzo**m**   |
|            31 | spibaklik  | spibaklo**k**  | spibaklo**m**  | spibaklo**k**  | spibaklo**m**  |
|            32 | tiregzik   | tiregze**k**   | tiregze**m**   | tiregze**k**   | tiregze**m**   |
|            33 | fretszik   | fretsze**k**   | fretsze**m**   | fretsze**k**   | fretsze**m**   |
|            34 | narjárzik  | narjárzo**k**  | narjárzo**m**  | narjárzo**k**  | narjárzo**m**  |
|            35 | spatárzik  | spatárzo**k**  | spatárzo**m**  | spatárzo**k**  | spatárzo**m**  |
|            36 | málapszik  | málapszo**k**  | málapszo**m**  | málapszo**k**  | málapszo**m**  |
|            37 | lorjódzik  | lorjódzo**k**  | lorjódzo**m**  | lorjódzo**k**  | lorjódzo**m**  |
|            38 | ribegzik   | ribegze**k**   | ribegze**m**   | ribegze**k**   | ribegze**m**   |
|            39 | flagárzik  | flagárzo**k**  | flagárzo**m**  | flagárzo**k**  | flagárzo**m**  |
|            40 | frilegszik | frilegsze**m** | frilegsze**m** | frilegsze**k** | frilegsze**k** |
|            41 | sprökszik  | sprökszö**m**  | sprökszö**m**  | sprökszö**k**  | sprökszö**k**  |
|            42 | brágaklik  | brágaklo**m**  | brágaklo**m**  | brágaklo**k**  | brágaklo**k**  |
|            43 | prilegzik  | prilegze**m**  | prilegze**m**  | prilegze**k**  | prilegze**k**  |
|            44 | drubódzik  | drubódzo**m**  | drubódzo**m**  | drubódzo**k**  | drubódzo**k**  |
|            45 | siladzik   | siladzo**m**   | siladzo**m**   | siladzo**k**   | siladzo**k**   |
|            46 | sojzik     | sojzo**m**     | sojzo**m**     | sojzo**k**     | sojzo**k**     |
|            47 | piragszik  | piragszo**m**  | piragszo**m**  | piragszo**k**  | piragszo**k**  |
|            48 | driládzik  | driládzo**m**  | driládzo**m**  | driládzo**k**  | driládzo**k**  |
|            49 | ribzik     | ribzo**m**     | ribzo**m**     | ribzo**k**     | ribzo**k**     |
|            50 | herégszik  | herégsze**m**  | herégsze**m**  | herégsze**k**  | herégsze**k**  |
|            51 | sprogzik   | sprogzo**m**   | sprogzo**m**   | sprogzo**k**   | sprogzo**k**   |
|            52 | vilakszik  | vilakszo**m**  | vilakszo**m**  | vilakszo**k**  | vilakszo**k**  |
|            53 | pivegszik  | pivegsze**m**  | pivegsze**m**  | pivegsze**k**  | pivegsze**k**  |
|            54 | spilágzik  | spilágzo**m**  | spilágzo**m**  | spilágzo**k**  | spilágzo**k**  |

Levelling list 1 across co-player types

The matching task has a post test to check whether the participant
maintains the patterns learned in the matching task. Participants were
allocated to lists and conditions at random. If a participant had list
1/2/3 in the matching game, they had list 2/3/1 in the post test.

There were three lists and all four co-player types played all lists,
resulting in 12 unique lists in total. The pre-registration specifies 50
trials, the experiment had 54 trials.

Participants saw words in a random order in both the matching game and
the post test. The order of variants in each trial was also randomised.

## Participants

182 participants completed the matching game in late 2022 and early
2023. Sample size was pre-set to 7 per unique list (12 lists) or 21 per
condition (high / low x typical / reversed x levelling / vowel
deletion). 5participants who matched our exclusion criteria (see below)
were excluded. Then, we kept data from the first seven participants in
each list (excluding an additional 9 participants), resulting in a final
total of 7 x 3 x 2 x 2 x 2 = 168 participants, 111 women. Median age was
22. Participants completed the task for course credit.

## Procedure

The task was coded in [jspsych](jspsych.org) and hosted on Pavlovia.
Participants were instructed that they would see prompts that contain
words that might be unfamiliar to them. They were asked to pick a
suffixed form to finish the prompt in a way that seems most natural or
familiar to them. They were informed that different people might have
different preferences and there are no correct or incorrect answers. An
example preceded the trials. Participants used the keyboard to respond.

Participants were also instructed that they play this game with another
player. The other player always has the same options as them. Their
joint task is to choose the same option in each trial. Participants were
instructed that, in order to do so, they have to pay attention to the
other player’s answers and guess how they will choose next. If they get
it right, they get a point.

After an example trial, participants were informed that they would be
connected to the other player. This was followed by a delay of circa
five seconds during which the message “connecting to the other player…”
was seen on the screen, followed by a brief message saying “connection
successful”.

Each matching game trial had the same forced-choice format as the
baseline task, except that, instead of the keyboard, participants had to
click on buttons to make a choice. In each trial, the other player would
“think” for a random interval within 3-9 seconds, during which the
message “the other player is thinking” was visible above the trial
prompt. If the participant made a choice during this time, they had to
wait for the other player to “finish” the trial. If the participant did
not make a choice during this time, the message “the other player made a
choice” became visible. Once the participant made their choice, they
were informed about the co-player’s choice and whether it matched their
own choice. A match was rewarded with a green tick and the message “you
matched the other player”. A mismatch was rewarded with a red X and the
message “you did not match the other player”. In addition, a match
awarded a point and the current score of the player and the co-player
was displayed on the screen.

After the matching game, the participant was instructed to do the same
task, to finish prompts, but, this time, without the co-player. Once the
participant finished this post test, a message informed them of their
final score. Score had no effect on the game or on participant
compensation.

## Hypotheses

We expected people to (1) learn in the matching game and become better
at predicting the co-players answers. We expected (2) learning to
persist in the post test so that patterns in the post test are similar
to patterns in the matching game. We expected that (3) learning will be
easier and persistent effects stronger for the socially salient
variation, levelling, as compared to vowel deletion.

We do not know the priors of the participants. Some of them likely have
a high or low preference for variant 1 themselves. We do know the priors
for the target words, based on the baseline task.

In terms of (1) learning, we can see how well participants converge to
various co-players across levelling or vowel deletion stimuli. In terms
of (2) persistent effects, we can look at differences in post test
responses across target words and participants in the various co-player
conditions (high / low, typical / reversed, levelling / vowel deletion).

## Analysis

Participants who only picked the left button or the right button were to
be excluded from the data. No participants met this pre-registered
exclusion criterion. Participants whose finished either the matching
game or the post test slower than the median completion time plus three
times the mean absolute deviation of completion times were excluded.
Individual trials that had a longer response time than the median
response time plus three times the mean absolute deviation of response
times were also excluded. Median and mean absolute deviation were
calculated separately for type of variation (levelling / vowel deletion)
and phase (matching game or post test).

1237 trials or 6.8176808% of all trials were dropped. This is a
relatively high number. This is likely both because the task was
difficult and because participants completed it in an uncontrolled
environment, both contributing to higher variability. Participants
completed the matching game and the post test in 10-15 minutes.

### Matching game

We fit a generalised additive mixed model ([Wood
2023](https://cran.r-project.org/web/packages/mgcv/index.html)) on the
matching data. The outcome variable was agreement with the co-player.
The predictors were trial index, rate of use of variant 1 (high / low),
distribution of use of variant 1 (typical / atypical), type of variation
(levelling / vowel deletion), and the log odds of the number of variant
1 and variant 2 responses to the target form in the baseline task.
Grouping factors were participant and target form. Models were fit using
restricted maximum likelihood, a binomial error distribution, and a
logit link function. We did not drop terms and instead explored all
interactions and used goodness-of-fit tests and AIC for model comparison
([van Rij
2022](https://www.rdocumentation.org/packages/itsadug/versions/2.4.1)).

The best model had a linear effect of trial index and a quadratic effect
of baseline log odds of variant 1 / variant 2. We re-fit it as a
generalised linear mixed model with a linear effect of trial index and a
linear effect of the absolute baseline log odds of variant 1 and variant
2. We report this model below.

### Post test

We fit a generalised linear mixed model ([Bates
2023](https://cran.r-project.org/web/packages/lme4/index.html)) on the
post test data. The outcome variable was agreement with the co-player.
The predictors were rate of use of variant 1 (high / low), distribution
of use of variant 1 (typical / atypical), type of variation (levelling /
vowel deletion), and the log odds of the number of variant 1 and variant
2 responses to the target form in the baseline task. Grouping factors
were participant and target form. Models were fit using restricted
maximum likelihood, a binomial error distribution, and a logit link
function. The post test analysis was pre-registered. Grouping factors
were participant and target form. Models were fit using maximum
likelihood, a binomial error distribution, and a logit link function. We
did not drop terms and isntead explored all interactions and used
goodness-of-fit tests, AIC, and Bayes Factors for model comparison. We
used the performance package ([Lüdecke
2023](https://cran.r-project.org/web/packages/performance/index.html))
to check model health and to compare models.

The model of the matching data was largely explorative. The model of the
post test data was pre-registered, down to interactions and random
effect structure, with the notable difference that the final analysis
uses frequentist, rather than Bayesian methods. The main advantage of
Bayesian statistics is that frequentist models often result in errors of
magnitude by overfitting the data. The use of pre-registered sample
size, exclusion criteria, and model structure, along with Bayes Factors
to detect overfitting, largely mitigates this problem.

The best model had an interaction of baseline log odds and co-player
typicality. Random slopes did not improve this model. We report this
model below.

## Results

### Matching game

`match ~ 1 + reg_rate + reg_dist * scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base)`

<figure>
<img src="~/Github/Racz2024/analysis/esp_analysis/preds/pred1.png"
alt="Matching 1" />
<figcaption aria-hidden="true">Matching 1</figcaption>
</figure>

<figure>
<img src="~/Github/Racz2024/analysis/esp_analysis/preds/pred2.png"
alt="Matching 2" />
<figcaption aria-hidden="true">Matching 2</figcaption>
</figure>

<figure>
<img src="~/Github/Racz2024/analysis/esp_analysis/preds/pred3.png"
alt="Matching 3" />
<figcaption aria-hidden="true">Matching 3</figcaption>
</figure>

### Post test

`picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1 + 1|part_id) + (1|base)`

<figure>
<img src="~/Github/Racz2024/analysis/esp_analysis/preds/pred4.png"
alt="post 1" />
<figcaption aria-hidden="true">post 1</figcaption>
</figure>

<figure>
<img src="~/Github/Racz2024/analysis/esp_analysis/preds/pred5.png"
alt="post 2" />
<figcaption aria-hidden="true">post 2</figcaption>
</figure>

<figure>
<img src="~/Github/Racz2024/analysis/esp_analysis/preds/pred6.png"
alt="post 3" />
<figcaption aria-hidden="true">post 3</figcaption>
</figure>

## Data and code

Code used to set up and process the experiment is in `scripts/esp_exp`.
The code to run the experiment is in `interface/hesp.zip`. Code to fit
models is in `analysis/esp_analysis/esp_analysis.R`. Code to visualise
data is in `analysis/esp_analysis/esp_pred_viz.R`. Raw data are in
`exp_data/raw`, tidy data are in `exp_data/baseline`.
