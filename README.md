# LightroomStuff
Disse filene kan brukes til å knytte Lightroom sammen med informasjon og tjenester fra Artsdatabanken.

Artsdatabank_LR_Keyword_Hierarchy.r : Oppskriften bak det å gå fra Artsdabanken sin info til å lage en fil som kan importeres til Lightroom.



LR_Keywords.txt: Filen man får fra 'Artsdatabank_LR_Keyword_Hierarchy.r'. Denne kan lastes ned og importeres inn til Lightroom gjennom Library--> 'Metadata'--> 'Import keywords'. Da vil alle Norges arter innen plante-, dyre- og soppriket ligge under nøkkelordet 'Taksonomi' i 'Keywords'-panelet.



ExportFromLightroomToArtsobservasjoner.r: Hvis man har bilder med GPS-info og keywords som tilsvarer norske artsnavn (eller fra hierarkiet nevnt over) i Lightroom kan man bruke dette skriptet til å lage en tekstfil som kan lastes opp til Artsobservasjoner. I Artsobservasjoner kan denne tekstfilenn klippes-limes rett inn i.

Fremtidige steg: 
- Gjør denne koden uavhengig av R? Implementere den i f.eks. PowerShell som finnes i alle pc-er som default.
- Inkl. engelske artsnavn?


ps! Beklager for eventuelle engelske kommentarer i .r-filene. Programmeringsdelen av hjernen min er engelsk.

