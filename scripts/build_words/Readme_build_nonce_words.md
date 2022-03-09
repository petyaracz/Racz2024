## Script logic

1. build_real_word_lists
2. build_tests
3. build_nonce_word_lists

## Basic setup

I picked three types of morphological variation, created a reference set for each type consisting of variable forms in the Hungarian webcorpus, and then generated nonce word lists based on the reference sets.

## Three variations

1. lakok/lakom

The definite form is used in the indefinite in 1sg.present verbs in educated colloquial Hungarian. More prevalent with some noun-to-verb derivational suffixes (-odik, -zik, -lik). See Rácz 2019. Note that Hungarian verbs is a closed class. 

|            | \multicolumn{3}{c}{regular verb} | \multicolumn{3}{c}{(-ik) verb} |
|------------|----------------------------------|--------------------------------|
|            | \sc 1sg.indef                    | \sc 2sg.indef                  | \sc 3sg.indef           | \sc 1sg.indef                                 | \sc 2sg.indef                | \sc 3sg.indef                              |
| {\sc ind}  | \textipa{fe:syl\o{}k }           | \textipa{fe:syls }             | \textipa{fe:syl }       | \colorbox{gray}{\textipa{fe:sylk\o{}d\o{}m} } | \textipa{fe:sylk\o{}d\o{}l } | \colorbox{gray}{\textipa{fe:sylk\o{}dik} } |
| {\sc imp}  | \textipa{fe:syljEk }             | \textipa{fe:sylj }             | \textipa{fe:sylj\o{}n } | \textipa{fe:sylk\o{}djEm }                    | \textipa{fe:sylk\o{}djel }   | \textipa{fe:sylk\o{}djek }                 |
| {\sc cond} | \textipa{fe:sylnek }             | \textipa{fe:sylnel }           | \textipa{fe:sylnE }     | \textipa{fe:sylk\o{}dnem }                    | \textipa{fe:sylk\o{}dnel }   | \textipa{fe:sylk\o{}dnek }                 |

2. dzsungelban/dzsungelben

Nominal post-positions usually agree with stem in front/back vowel and vowel roundedness. There are front and back stem vowels. Some stem vowels (i,í,e,é) are neutral. (e) and increasingly (é) are front vowels. As a result, back vowel + (e,é) stems can vary. See https://doi.org/10.3765/amp.v8i0.4750

3. cselekszik/cselekedik

A Hungarian verb stem can end in CC or CVC. Verbal suffixes can be C- or V-initial. Some stems vary in that a CVC stem is realised with a C-initial suffix and a CC stem is realised with a V-initial suffix. This is likely because we have a CVCV sequence in the former case and a VCCV sequence in the latter case, both of which are phonotactically well-formed (Some verbs always have CVC stems even when this is not phonotactically necessary, other verbs are defective, only have CC stems and some C-initial suffixed forms do not exist.)

| {\em class} | {\sc 3sg.ind} (no suf.) | {\sc 1sg.ind} (V-suf.)   | {\sc 3sg.imp} (C-suf.)        | {\sc 3sg.cond} (C/V-suf.)        | {\em gloss} |
|-------------|-------------------------|--------------------------|-------------------------------|----------------------------------|-------------|
| 1 stable    | \textipa{ a:pol }       | \textipa{ a:polok }      | \textipa{ a:poljon }          | \textipa{ a:poln6 }              | nurse       |
| 2 weak      | \textipa{ S\o{}p\o{}r } | \textipa{ S\o{}pr\o{}k } | \textipa{ S\o{}p\o{}rj\o{}n } | \textipa{ S\o{}p\o{}rnE }        | sweep       |
| 3 variable  | \textipa{ fyrdik }      | \textipa{ fyrd\o{}k }    | \textipa{ fyr\o{}dj\o{}n }    | \textipa{ fyr\o{}dnE / fyrdEnE } | bathe       |
| 4 no vowel  | \textipa{ hord }        | \textipa{ hordok }       | \textipa{ hordjon }           | \textipa{ hord6n6 }              | carry       |
| 5 defective | \textipa{ Siklik }      | \textipa{ Siklok }       | \textipa{ *Sikljon }          | \textipa{ Sikl6n6 }              | slide       |


Some epenthetic stems are monomorphemic (fürdik, ugrik: ugrani - ugornak - ugranak) but most are formed with a small set of productive derivational suffixes.

## Extracting variable patterns from the webcorpus

I used the morphologically disambiguated and pos tagged Hungarian Webcorpus 2 (https://hlt.bme.hu/en/resources/webcorpus2) to compile a form frequency list. I used this list to calculate the log odds of variation in the (i) definite/indefinite suffix used in 1sg.indef -ik verb forms, (ii) the front/back suffix used with variable noun stems, and (iii) CVC/CC forms of variable verb+suffix combinations.

For the lakok/lakom 1sg.indef -ik verb forms, there is, in practice, only one variable morphological exponent (whether the 1sg.indef form is realised with the indefinite or the definite suffix). For the dzsungelban/dzsungelben variable noun stems, I selected five locative nominal post-positions known to vary: the inessive, the illative, the adessive, the dative, and the sublative. For the cselekszik/cselekedik variable verb stems, I selected five verbal suffixes known to vary: the infinitive, the Pres.NDef.3Pl, the Past.NDef.3Pl, the Cond.NDef.3Sg, and the Pres.NDef.2Pl.

For the nouns, variation does not affect the stem itself, and so identifying variable stems with the five post-positions was straightforward. For the verbs, instead of searching for a list of possibly variable lemmata, each possible variant was built and then grepped individually to make sure the scoop was as large as possible.

All three variable patterns are strongly correlated with cross-referenced sets from the first Hungarian Webcorpus (http://mokk.bme.hu/en/resources/webcorpus/). 

## Generating nonce words

An ngram model was built to generate nonce words from pre-defined constituent parts in relevant real word patterns. Nouns were generated based on existing bisyllabic variable noun stems (where a back vowel is followed by e/é). Verbs were generated based on existing bi- and trisyllabic verb stems ending in one of four productive derivational verbal suffixes: -lik, -odik, -szik, -zik. The verb class is closed in Hungarian and new verbs are formed using derivational suffixes: Google - Guglizik, Facebook - Facebookol, etc. Native speakers are far more likely to accept nonce verbs that have recognisable derivational endings.

Existing words were broken up into onset-rhyme couplets and then these were freely recombined to build nonce forms. For verbs, I added some complex onsets to enlarge the set of possible combinations. The recombined set was filtered to make sure it (a) excludes uncommon constituents (e.g. "yiddish" is a Hungarian word but most Hungarian words do not have a "y-" onset) and (b) observes restrictions on syllables and consonant sequences in monomorphemic forms. Most of these restrictions were only relevant for the nouns. If a participant takes the phonotactic cues to parse a nonce noun as a compound, they will be very likely to only consider the second constituent in selecting a suffix (so that all suffixes will be front-only). For verbs, the derivational suffixes constitute a morphemic boundary. 

Nonce forms were sampled across relevant dimensions (nouns: is the second vowel e/é; -ik verbs: number of syllables, epenthetic verbs: number of syllables and type of derivational suffix) and then the sampled lists were filtered to make sure (a) they had sufficient edit distance from existing forms and (b) did not start or end with a string identical to an existing word and (c) had sufficient edit distance from one another. Final samples were hand-filtered.

## Generating final forms

The final sets consisted of 162 forms per variation type (noun / -ik verb / epenethetic verb). All nouns in the dzsungelban/dzsungelben set were bisyllabic, consisting of a back vowel and front e/é. All verbs in the lakok/lakom set were mono- or bisyllabic and ended in -lik (see 'csuklik'), -szik (see 'emlékszik'), or -zik (see 'éhezik'). All verbs in the cselekszik/cselekedik set were mono- or bisyllabic and had one of three alterations: -lik / -ozik (see porlik/porozik, common for loans, see squashol / squashozik), -szik / -dik (see cselekszik/cselekedik), or -zik / -zik (see habzik / habozik).

For the dzsungelban/dzsungelben and cselekszik/cselekedik patterns, a suffix was chosen at random for each form. For the lakok/lakom pattern, variation was restricted to one suffix, so this filtering was not necessary.