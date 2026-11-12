# KoCoLog



Viewer für Logdateien aus einer KoCoBox.



Die Logs im Webfrontend zu lesen ist IMHO ziemlich bescheiden und sie nach einem Export in einem Texteditor zu sichten, ist auch nicht viel besser.
Also habe ich dieses kleine Tool geschrieben, welches ein paar nette Features, wie eine Filterfunktion und Highlights für Warnung/Fehler/fatale Fehler, hat.



Es wurde in FreePascal mit der Lazarus-IDE, ohne zusätzliche Komponenten, geschrieben.
Da es sich um ein privates Projekt handelt, gibt es von mir auch keinen Support.



## Benutzung



Das Programm starten und mit "Datei/öffnen" eine .log-Datei auswählen oder die .log-Datei als Kommandozeilenparameter mitgeben, um eine Datei anzuzeigen.
Zeit, Rubrik, Typ und Parameter werden chronologisch, tabellarisch aufgelistet. Es kann über "Ansicht/Neue Einträge zu erst" eingestellt werden, ob in auf- oder absteigender Reihenfolge.
Ist der Typ ERR, WARN oder FATAL, wird die gesamte Zeile fablich markiert.



Mit "Ansicht/Filter" lassen sich die Einträge entsprechend filtern.

Unter "Auswertung/Verlauf" ist eine grafische Übersicht, über aufgetretene Fehler und Warnungen, zugänglich.

