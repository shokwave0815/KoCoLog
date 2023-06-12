{******************************************************************************
KoCoLog - Programm zum anzeigen der .log-dateien einer KoCoBox.

In der Weboberfläche der KoCoBox ist das scrollen in den Logs sehr mühsam und
die heruntergeladenen Logfiles sind im Texteditor ziemlich unübersichtlich.

Features: -einfache Filterfunktion
          -farbliche Hervorhebung von Warnungen, Fehern und fatalen Fehlern
          -Kommandozeilenparameter zur Übergabe des Dateinamens
          -auf- oder absteigende chronologische Reihenfolge
          -konfigurierbare Fenster- und Spaltengröße
          -Anzeige der Anzahl der (gefilterten) Datensätze
          -graphische Auswertung der Warnungen, Fehler und fatalen Fehler
******************************************************************************}
program kocolog;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
   {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    klmain,
    klfilter,
    kltextutil,
    klcourse { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TForm_Main, Form_Main);
    Application.CreateForm(TForm_Filter, Form_Filter);
    Application.CreateForm(TForm_Course, Form_Course);
    Application.Run;
end.
