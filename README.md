Latex for equations can be edited on overleaf at the following link: https://www.overleaf.com/9746813331cmxhjfjmcwhk.


# 2020-04-29

Start with a simple model of Covid spread, combined with a simple
model of how people are tested and how tests are reported.

Ask what would happen if people fit models to test reports, based on
either collection date, or report date.

Factors affecting testing include capacity, weekdays and holidays,
testing guidelines, and test-seeking behaviour (how much covid and
non-covid illness, how much alarm there is in the population)

Eventually, take simple methods of correcting or fitting that account
for observed testing patterns.

## A simple COVID model

Presumably we can choose from something that's ready to go?

Or maybe not? Come up with a box structure and build a simple
simulation engine on top of it.

Step 1 is to make a box model that we like (by referring to others' box models).
* Maybe build the observation process right on top of that?

SEIR seems necessary. Do we need anything else?
* asymptomatic, pre-symptomatic
* Maybe something like S => E => P, and then a split into symptomatic
and asymptomatic infectious individuals, followed by R
* S E P I_a/I_s R
* This seems to already be a thing (Stanford, McMaster)

Box model could incorporate simple phenomenological changes in R(t)
for example, and see if we can pull this out.

## A simple observation model

The simplest starting point here is just a delay kernel. We could also
think about accounting explicitly for backlogs

Sample collection (disease based, travel based, tracing based, panic based)
* Maybe start after the travel-important period?

Testing and reporting
* Availability of tests and activeness of reporting could be random or
imposed from outside
* Key might be to have explicit boxes of people who wish to be tested;
have been tested but not reported

How should we add this to the box model?

## Brainstorming

Resources:
* https://docs.google.com/spreadsheets/d/1hUZlVDPfa5C8KgURoP_3dAiUQgI6rdb7A5e_g8NcPaY
* https://github.com/ICI3D/modelTaxonomy_SI

Future points:
* Quarantine and isolation?
* Social distancing and interactions with testing

### Next steps

SMART subgroup
* Disease box diagram COB Thursday and share with bigger group
* Disease plus observation box diagram COB Friday and share with bigger group

Juliet
* Set up a repo

Amandla
* ngawethu!
