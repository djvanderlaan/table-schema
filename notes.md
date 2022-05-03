

In principe gaan we uit van het volgende meta-data-formaat: https://specs.frictionlessdata.io/table-schema/ . Maar er zijn een aantal features die daar nog ontbreken en een aantal features die misschien initieel niet nodig zijn om te ondersteunen.

Volgende formaten zouden initieel minimaal ondersteund moeten worden:
- number
- integer
- boolean
- date
- datetime
- year
- yearmonth

Mogelijk nodig maar niet als standaard type ondersteunt:
- yearquarter


Missing values worden op bestandsniveau gedefinieerd. Dat is op zich logisch. Ik zie alleen een probleem bij fixed-width bestanden waarbij missende waardes soms worden aangegeven met `----`. Het aantal `-` hangt dan af van de veldbreedte. Optie: geef bij lijst met missende waardes alle opties die in het bestand voor komen, dus `['-', '--', '---', ...]`.

Belangrijke missende dingen in table-schema:
- codelijsten
- waardes die een missende waarde aangeven (bijv. `9`=`onbekend`). In veel analyses in bijv. R of pandas wil je die als `NA` coderen. 
- veldbreedtes voor als je het bestand als fixed width moet opslaan of inlezen. Lijkt me handig om dit ook (optioneel) op te slaan als het bestand niet als fixed width is opgeslagen. Op die manier kan bijv. makkelijk een bestand gemaakt worden voor het DSC of StatLine.

