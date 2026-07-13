unit klmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, ComCtrls,
  ExtCtrls, StdCtrls, klfilter, Types, INIFiles, kltextutil, klcourse;

const
  MY_VERSION = 'KoCoLog V0.7.17';

type

  { TForm_Main }

  TForm_Main = class(TForm)
    GroupBox1: TGroupBox;
    Label_Time: TLabel;
    Label_Topic: TLabel;
    Label_Type: TLabel;
    Label_Param: TLabel;
    Label_Type_Content: TLabel;
    Label_Topic_Content: TLabel;
    Label_Time_Content: TLabel;
    MainMenu: TMainMenu;
    Memo_Param: TMemo;
    MenuItem1: TMenuItem;
    MenIt_Open: TMenuItem;
    MenIt_Close: TMenuItem;
    MenIt_Descending: TMenuItem;
    MenuItem2: TMenuItem;
    MenIt_About: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenIt_Filter: TMenuItem;
    MenIt_Course: TMenuItem;
    N1: TMenuItem;
    OpDi: TOpenDialog;
    StringGrid_Main: TStringGrid;
    StatusBar: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenIt_CourseClick(Sender: TObject);
    procedure MenIt_DescendingClick(Sender: TObject);
    procedure MenIt_OpenClick(Sender: TObject);
    procedure MenIt_CloseClick(Sender: TObject);
    procedure MenIt_FilterClick(Sender: TObject);
    procedure MenIt_AboutClick(Sender: TObject);
    procedure StringGrid_MainDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure StringGrid_MainSelection(Sender: TObject; aCol, aRow: integer);
  private
    FIsStartup: boolean;  //signalisiert ob gerade der Programmstart ausgeführt wird
    FCurrentFile: string;  //Dateiname und Pfad der aktuell geöffneten Datei
    FCfgFileName: string;  //Dateiname und Pfad der Konfigurationsdatei
    FCfgINI: TIniFile;
    FMyHeight, FMyWidth: integer; //Höhe und Breite von Form_Main, um sie nach Neustart wiederherzustellen
    procedure ReadConfig();
    procedure SaveConfig();
    procedure OpenFile(AFilename: string);
  public

  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

{ TForm_Main }

{******************************************************************************
Menü: Beenden
******************************************************************************}
procedure TForm_Main.MenIt_CloseClick(Sender: TObject);
begin
  Close();
end;

{******************************************************************************
Menü: Filter
******************************************************************************}
procedure TForm_Main.MenIt_FilterClick(Sender: TObject);
var
  WasFilteredBefore: boolean;
begin
  WasFilteredBefore := Form_Filter.IsFiltered;

  //Aufruf Form_Filter und setzen von isFiltered
  Form_Filter.IsFiltered := Form_Filter.ShowModal = mrOk;

  //Datei neu laden, wenn jetzt gefiltert wird oder vorher gefiltert wurde und jetzt nicht
  if (Form_Filter.IsFiltered or (WasFilteredBefore <> Form_Filter.IsFiltered)) then
  begin
    OpenFile(FCurrentFile);
  end;
end;

{******************************************************************************
Menü: Über KoCoLog
******************************************************************************}
procedure TForm_Main.MenIt_AboutClick(Sender: TObject);
begin
  MessageDlg('Über KoCoLog', MY_VERSION + LineEnding + '©2021-2026 Ingo Steiniger' +
    LineEnding + LineEnding + 'Programm zum anzeigen der .log-Dateien einer KoCoBox.',
    mtInformation, [mbOK], 0);
end;

{******************************************************************************
Ereignis: Zelle von StringGrid_Main zeichnen
******************************************************************************}
procedure TForm_Main.StringGrid_MainDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);
begin
  if (not (gdFixed in aState)) then
  begin
    StringGrid_Main.canvas.Brush.Color := clWhite;
    StringGrid_Main.Canvas.Font.Color := clBlack;

    case StringGrid_Main.Cells[2, aRow] of
      'WARN': begin
        StringGrid_Main.canvas.Brush.Color := TColor($CCFFFF);
      end;
      'ERR': begin
        StringGrid_Main.canvas.Brush.Color := TColor($CCCCFF);
      end;
      'FATAL': begin
        StringGrid_Main.canvas.Brush.Color := TColor($FFAAFF);
      end;
    end;

    StringGrid_Main.Canvas.FillRect(arect);
    StringGrid_Main.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, StringGrid_Main.Cells[aCol, aRow]);
    StringGrid_Main.Canvas.FrameRect(aRect);
  end;
end;

procedure TForm_Main.StringGrid_MainSelection(Sender: TObject; aCol, aRow: integer);
begin
  Label_Time_Content.Caption := StringGrid_Main.Cells[0, aRow];
  Label_Topic_Content.Caption := StringGrid_Main.Cells[1, aRow];
  Label_Type_Content.Caption := StringGrid_Main.Cells[2, aRow];
  Memo_Param.Text := StringGrid_Main.Cells[3, aRow];
  aCol := aCol;
end;

{******************************************************************************
Prozedur: Konfigurationsdatei lesen
******************************************************************************}
procedure TForm_Main.ReadConfig();
begin
  FCfgINI := TINIFile.Create(FCfgFileName);

  //Dimension und Position der MainForm
  Top := Scale96ToScreen(FCfgINI.ReadInteger('Window', 'Top', 100));
  Left := Scale96ToScreen(FCfgINI.ReadInteger('Window', 'Left', 200));
  Width := Scale96ToForm(FCfgINI.ReadInteger('Window', 'Width', 800));
  Height := Scale96ToForm(FCfgINI.ReadInteger('Window', 'Height', 600));
  WindowState := TWindowState(FCfgINI.ReadInteger('Window', 'State', 0));
  FMyHeight := Height;
  FMyWidth := Width;

  //Größe der Spalten des StringGrid
  StringGrid_Main.Columns.Items[0].Width :=
    Scale96ToForm(FCfgINI.ReadInteger('SG', '0', 150));
  StringGrid_Main.Columns.Items[1].Width :=
    Scale96ToForm(FCfgINI.ReadInteger('SG', '1', 300));
  StringGrid_Main.Columns.Items[2].Width :=
    Scale96ToForm(FCfgINI.ReadInteger('SG', '2', 64));
  StringGrid_Main.Columns.Items[3].Width :=
    Scale96ToForm(FCfgINI.ReadInteger('SG', '3', 1000));

  //Option auf-/absteigende Reihenfolge
  MenIt_Descending.Checked := FCfgINI.ReadBool('Option', 'Asc', False);

  FreeAndNil(FCfgINI);
end;

{******************************************************************************
Prozedur: Konfigurationsdatei speichern
******************************************************************************}
procedure TForm_Main.SaveConfig();
begin
  FCfgINI := TINIFile.Create(FCfgFileName);

  //Dimension und Position der MainForm
  if (WindowState = wsNormal) then //nicht speichern, wenn maximiert, minimiert
  begin
    FCfgINI.WriteInteger('Window', 'Top', ScaleScreenTo96(Top));
    FCfgINI.WriteInteger('Window', 'Left', ScaleScreenTo96(Left));
    FCfgINI.WriteInteger('Window', 'Width', ScaleFormTo96(Width));
    FCfgINI.WriteInteger('Window', 'Height', ScaleFormTo96(Height));
  end;
  FCfgINI.WriteInteger('Window', 'State', Ord(WindowState));

  //Größe der Spalten des StringGrid
  FCfgINI.WriteInteger('SG', '0',
    ScaleFormTo96(StringGrid_Main.Columns.Items[0].Width));
  FCfgINI.WriteInteger('SG', '1',
    ScaleFormTo96(StringGrid_Main.Columns.Items[1].Width));
  FCfgINI.WriteInteger('SG', '2',
    ScaleFormTo96(StringGrid_Main.Columns.Items[2].Width));
  FCfgINI.WriteInteger('SG', '3',
    ScaleFormTo96(StringGrid_Main.Columns.Items[3].Width));


  //Option auf-/absteigende Reihenfolge
  FCfgINI.WriteBool('Option', 'Asc', MenIt_Descending.Checked);

  FreeAndNil(FCfgINI);
end;

{*******************************************************************************
Prozedur: Datei öffnen und Anzahl Einträge in der Statusbar anzeigen
Beschreibung: Vor- und Nachbereitung zum öffenen der Datei
Parameter: aFilename = vollständiger Dateiname mit Pfad
*******************************************************************************}
procedure TForm_Main.OpenFile(AFilename: string);
begin
  if (FileExists(AFilename)) then
  begin
    FCurrentFile := AFilename;
    Caption := MY_VERSION + ' - ' + AFilename;
    StatusBar.SimpleText := 'verarbeite Daten...';
    Application.ProcessMessages();

    if (ReadFile(AFilename, StringGrid_Main, MenIt_Descending.Checked)) then
    begin
      if (StringGrid_Main.RowCount > 1) then
      begin
        StringGrid_MainSelection(Self, 0, 1);
      end;
      StatusBar.SimpleText := 'Anzahl Einträge: ' + Format('%.0N', [StringGrid_Main.RowCount / 1]);
      if (Form_Filter.IsFiltered) then
      begin
        StatusBar.SimpleText := StatusBar.SimpleText + '(gefiltert)';
      end;
    end else
    begin
      StatusBar.SimpleText := 'Fehler beim lesen der Datei "' + AFilename + '"';
    end;
  end else
  begin
    if (AFilename <> '') then
    begin
      MessageDlg('Fehler', 'Fehler: Die Datei "' + AFilename + '" konnte nicht gefunden werden!' +
        LineEnding + 'Prüfen Sie ob die Datei existiert und ob Sie Leserechte, für die Datei haben.',
        mtError, [mbOK], 0);
    end;
  end;
end;

{******************************************************************************
Menü: Datei/öffnen
******************************************************************************}
procedure TForm_Main.MenIt_OpenClick(Sender: TObject);
begin
  if (OpDi.Execute()) then
  begin
    OpenFile(OpDi.FileName);
  end;
end;

{******************************************************************************
Ereignis: Programmende
******************************************************************************}
procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig();
  CloseAction := caFree;
end;

{******************************************************************************
Ereignis:Programmstart
******************************************************************************}
procedure TForm_Main.FormCreate(Sender: TObject);
begin
  Caption := MY_VERSION;
  FIsStartup := True;
  FCurrentFile := '';

  FCfgFileName := GetAppConfigDir(False);
  ForceDirectories(FCfgFileName); //sicherstellen, dass das Verzeichnis existiert
  FCfgFileName += 'config.ini';
  ReadConfig();
end;

{*******************************************************************************
Ereignis: Programmstart #2
*******************************************************************************}
procedure TForm_Main.FormShow(Sender: TObject);
begin
  if (FIsStartup) then
  begin  //Aufrufparameter auswerten
    Form_Filter.IsFiltered := False;
    if (ParamCount > 0) then
    begin
      //Datei mit OpenFile() öffnen, damit der Filter aufgerufen wird!
      OpenFile(ParamStr(1));
    end;
    FIsStartup := False;
  end;
end;

{*******************************************************************************
Ereignis: Window State Change
Beschreibung: Stellt die korrekte Dimnsion des Fensters her, wenn es maximiert
              geöffnet wurde.
*******************************************************************************}
procedure TForm_Main.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsNormal) then
  begin
    Sleep(10); //ohne sleep reduziert sich das Fenster zur Unkenntlichkeit
    Height := FMyHeight;
    Width := FMyWidth;
  end;
end;

{*******************************************************************************
Menü: Verlauf; Zeichnet ein Diagramm
*******************************************************************************}
procedure TForm_Main.MenIt_CourseClick(Sender: TObject);
begin
  Form_Course.theStringGrid := StringGrid_Main;
  Form_Course.Show();
end;

{*******************************************************************************
Menü: chronologisch auf- oder absteigende Reihenfolge der Einträge
*******************************************************************************}
procedure TForm_Main.MenIt_DescendingClick(Sender: TObject);
begin
  MenIt_Descending.Checked := not MenIt_Descending.Checked;
  OpenFile(FCurrentFile);
end;

end.
