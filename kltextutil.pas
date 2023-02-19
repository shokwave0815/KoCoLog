unit kltextutil;

{$mode objfpc}{$H+}

interface

uses
	   Classes, SysUtils, Grids, klfilter, Dialogs;

function ReadFile(aFilename: string; aSG: TStringGrid; Descending: boolean): boolean;
function StringListToStringGrid(AllLines: TStringList; aSG: TStringGrid; Descending: boolean): boolean;
function ExtractEntry(aString: string): TStringList;

implementation

{******************************************************************************
Funktion: Textdatei einlesen
Beschreibung: Liest die Datei in eine StringList und übergibt sie an
              StringListToStringGrid()
Parameter: aFilename = vollständiger Dateiname mit Pfad
           aSG = StringGrid in das die Werte eingetragen werden sollen
           Descending = chronologische Reihenfolge der Werte
                       true = absteigend
                       false = aufsteigend
Rückgabe: True = erfolgreich
          False = Fehler aufgetreten
******************************************************************************}
function ReadFile(aFilename: string; aSG: TStringGrid; Descending: boolean): boolean;
var
	   AllLines: TStringList;
begin
	   Result   := True;
	   //Datei in StringList laden
	   AllLines := TStringList.Create();
	   try
					   try
									   AllLines.LoadFromFile(aFilename);
									   aSG.BeginUpdate();
									   Result := StringListToStringGrid(AllLines, aSG, Descending);
									   //Zeilen in Spalten aufteilen und ggf. filtern
									   aSG.EndUpdate();
					   except
									   on E: Exception do
									   begin
													   MessageDlg('Exception caught : ' + E.Message, mtError, [mbOK], 0);
													   Result := False;
									   end;
					   end;
	   finally
					   FreeAndNil(AllLines);
	   end;
end;

{******************************************************************************
Funktion: Einträge der Stringlist in das Stringrid eintragen
Beschreibung: Trägt die Werte der StringList(AllLines) in das StringGrid(aSG) ein
Parameter: AllLines = StringList mit der vollständigen, eingelesenen Datei
           aSG = StringGrid in das die Werte eingetragen werden sollen
           Descending = chronologische Reihenfolge der Werte
                       true = absteigend
                       false = aufsteigend
Rückgabe: True = erfolgreich
          False = Fehler aufgetreten
******************************************************************************}
function StringListToStringGrid(AllLines: TStringList; aSG: TStringGrid; Descending: boolean): boolean;
var
	   i, FilteredRowCount: integer;
	   SplittedLine: TStringList;

	   //Füllt das StringGrid
	   function AddData(): boolean;
	   var
					   j: integer;
	   begin
					   Result := True;
					   //aus einer Zeile die 4 interessanten Werte filtern
					   SplittedLine := ExtractEntry(AllLines[i]);

					   if (SplittedLine.Count = 4) then
					   begin
									   //wenn isFiltered=False wird matchFilter nicht abgefragt, da "not isFilterted" True ist
									   if (not Form_Filter.IsFiltered or Form_Filter.MatchAllFilters(SplittedLine)) then
									   begin
													   //die 4 Werte in die Spalten des StrinGrid eintragen
													   for j := 0 to 3 do
													   begin
																	   aSG.Cells[j, FilteredRowCount] := SplittedLine[j];
													   end;
													   //Reihen zählen, um das StringGrid auf die korrekte Größe zu korregieren, wenn gefiltert wird
													   Inc(FilteredRowCount);
									   end;
					   end else
					   begin
									   Result := False;
					   end;
					   FreeAndNil(SplittedLine);
	   end;

begin
	   Result := True;
	   FilteredRowCount := 1; //1, da die erste Zeile fix ist
	   //StringGrid leeren
	   aSG.RowCount := 1;
	   //Länge des StringGrid auf das Maximum festlegen.
	   aSG.RowCount := AllLines.Count + 1;

	   if (Descending) then
	   begin
					   //Daten in chronologisch aufsteigender Rheinfolge hinzufügen.
					   //In der Originaldatei sind die Einträge absteigend sortiert.
					   for i := AllLines.Count - 1 downto 0 do
					   begin
									   if (not AddData()) then
									   begin
													   Result := False;
													   break;
									   end;
					   end;
	   end else
	   begin
					   //Daten in chronologisch absteigender Rheinfolge hinzufügen
					   for i := 0 to AllLines.Count - 1 do
					   begin
									   if (not AddData()) then
									   begin
													   Result := False;
													   break;
									   end;
					   end;
	   end;

	   //Beschriftung der Spalten
	   aSG.Rows[0].AddStrings(['Zeit', 'Rubrik', 'Typ', 'Parameter'], False);
	   //Anzahl Zeilen korregieren
	   aSG.RowCount := FilteredRowCount;
end;

{******************************************************************************
Funktion: Aus einem String 4 Teilstrings extrahieren
Beschreibung: extrahiert aus einem String 4 Werte und gibt diese als StringList zurück
Parameter: aString = String mit den Werten
Rücgabe: Stringlist mit 4 Zeilen
******************************************************************************}
function ExtractEntry(aString: string): TStringList;

	   //Fügt den Text zwischen aKeyword und dem nächsten ';' bzw. '}' an Result an.
	   procedure AddToStringList(aKeyword: string);
	   var
					   aOffset, aLength: integer;
					   tmpString: string;
	   begin
					   aOffset := Pos(aKeyword, aString);
					   aLength := Length(aKeyword);

					   //"parameter" kann mehrere Werte, die durch ";" getrennt sind, enthalten ist aber von "{}" umschlossen
					   if (aKeyword = 'parameter={') then
					   begin
									   tmpString := copy(aString, aOffset + aLength, Pos('}', aString, aOffset) - aOffset - aLength);
					   end else
					   begin
									   tmpString := copy(aString, aOffset + aLength, Pos(';', aString, aOffset) - aOffset - aLength);
					   end;

					   //"parameter" kann auch leer sein, die anderen nicht
					   if ((length(tmpString) > 0) or (aKeyword = 'parameter={')) then
					   begin
									   Result.Add(tmpString);
					   end;
	   end;

begin
	   Result := TStringList.Create; //Wird in StringListToStringGrid freigegeben

	   //Datum Uhrzeit
	   AddToStringList('timestamp=');
	   //Rubrik
	   AddToStringList('topic=');
	   //Typ
	   AddToStringList('protocolSeverity=');
	   //Parameter
	   AddToStringList('parameter={');
end;

end.
