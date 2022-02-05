## Basic setup

I picked three types of morphological variation, created a reference set for each type consisting of variable forms in the Hungarian webcorpus, and then generated nonce word lists based on the reference sets.

## Three variations

1. ik

The definite form is used in the indefinite in 1sg.present verbs in educated colloquial Hungarian. More prevalent with some noun-to-verb derivational suffixes (-odik, -zik, -lik). See Rácz 2019. Note that Hungarian verbs is a closed class. 

|            | \multicolumn{3}{c}{regular verb} | \multicolumn{3}{c}{(-ik) verb} |
|------------|----------------------------------|--------------------------------|
|            | \sc 1sg.indef                    | \sc 2sg.indef                  | \sc 3sg.indef           | \sc 1sg.indef                                 | \sc 2sg.indef                | \sc 3sg.indef                              |
| {\sc ind}  | \textipa{fe:syl\o{}k }           | \textipa{fe:syls }             | \textipa{fe:syl }       | \colorbox{gray}{\textipa{fe:sylk\o{}d\o{}m} } | \textipa{fe:sylk\o{}d\o{}l } | \colorbox{gray}{\textipa{fe:sylk\o{}dik} } |
| {\sc imp}  | \textipa{fe:syljEk }             | \textipa{fe:sylj }             | \textipa{fe:sylj\o{}n } | \textipa{fe:sylk\o{}djEm }                    | \textipa{fe:sylk\o{}djel }   | \textipa{fe:sylk\o{}djek }                 |
| {\sc cond} | \textipa{fe:sylnek }             | \textipa{fe:sylnel }           | \textipa{fe:sylnE }     | \textipa{fe:sylk\o{}dnem }                    | \textipa{fe:sylk\o{}dnel }   | \textipa{fe:sylk\o{}dnek }                 |

2. vh

Nominal post-positions usually agree with stem in front/back vowel and vowel roundedness. There are front and back stem vowels. Some stem vowels (i,í,é) are neutral. (e) and increasingly (é) are front vowels. As a result, back vowel + (e,é) stems can vary. See https://doi.org/10.3765/amp.v8i0.4750

3. ep

A Hungarian verb stem can end in CC or CVC. Verbal suffixes can be C- or V-initial. Some stems vary in that a CVC stem is realised with a C-initial suffix and a CC stem is realised with a V-initial suffix. This is likely because we have a CVCV sequence in the former case and a VCCV sequence in the latter case, both of which are phonotactically well-formed (Some verbs always have CVC stems even when this is not phonotactically necessary, other verbs are defective, only have CC stems and some C-initial suffixed forms do not exist.)

| {\em class} | {\sc 3sg.ind} (no suf.) | {\sc 1sg.ind} (V-suf.)   | {\sc 3sg.imp} (C-suf.)        | {\sc 3sg.cond} (C/V-suf.)        | {\em gloss} |
|-------------|-------------------------|--------------------------|-------------------------------|----------------------------------|-------------|
| 1 stable    | \textipa{ a:pol }       | \textipa{ a:polok }      | \textipa{ a:poljon }          | \textipa{ a:poln6 }              | nurse       |
| 2 weak      | \textipa{ S\o{}p\o{}r } | \textipa{ S\o{}pr\o{}k } | \textipa{ S\o{}p\o{}rj\o{}n } | \textipa{ S\o{}p\o{}rnE }        | sweep       |
| 3 variable  | \textipa{ fyrdik }      | \textipa{ fyrd\o{}k }    | \textipa{ fyr\o{}dj\o{}n }    | \textipa{ fyr\o{}dnE / fyrdEnE } | bathe       |
| 4 no vowel  | \textipa{ hord }        | \textipa{ hordok }       | \textipa{ hordjon }           | \textipa{ hord6n6 }              | carry       |
| 5 defective | \textipa{ Siklik }      | \textipa{ Siklok }       | \textipa{ *Sikljon }          | \textipa{ Sikl6n6 }              | slide       |


## Extracting variable patterns from the webcorpus

I used the morphologically disambiguated and pos tagged Hungarian Webcorpus 2 (https://hlt.bme.hu/en/resources/webcorpus2) to compile a form frequency list. I used this list to calculate the log odds of variation in the (i) definite/indefinite suffix used in 1sg.indef -ik verb forms, (ii) the front/back suffix used with variable noun stems, and (iii) CVC/CC forms of variable verb+suffix combinations.

For the 1sg.indef -ik verb forms, there is, in practice, only one variable morphological exponent (whether the 1sg.indef form is realised with the indefinite or the definite suffix). For the variable noun stems, I selected five nominal post-positions known to vary. For the variable verb stems, I selected five verbal suffixes known to vary.

For the nouns, variation does not affect the stem itself, and so identifying variable stems with the five post-positions was straightforward. For the verbs, instead of searching for a list of possibly variable lemmata, each possible form was grepped individually to make sure the scoop was as large as possible.

All three variable patterns are strongly correlated with cross-referenced sets from the first Hungarian Webcorpus (http://mokk.bme.hu/en/resources/webcorpus/). 

## Generating nonce words

I built a simple ngram model of possible word-initial, mid-word, and word-final consonant clusters for lemmata of syllable count 1-3 separately for nouns and verbs. The lemmata had a certain threshold frequency in the corpus to exclude very rare and unusual-looking forms. The model was weighted by the token frequency of the units (so that initial "t" was much more common than initial "spr"). 

The building units of the models were filtered based on Hungarian phonotactics, expert advice and native speaker intuition.

Finally, nonce words matching the templates of the three reference sets (verbs ending in ik, bisyllabic variable noun stems, variable CVC/CC verb stems) were generated. The 3sg.indef was used as the prompt, the 1sg.indef (1sg.indef variation), the five post-positions (nouns), and the five verbal suffixes (CVC/CC verbs). Some combinations of stem and suffix are not attested in Hungarian, these were excluded from the final list of nonce words as well.
