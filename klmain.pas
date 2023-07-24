unit klmain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, ComCtrls,
    ExtCtrls, StdCtrls, klfilter, Types, INIFiles, kltextutil, klcourse;

const
    MY_VERSION = 'KoCoLog V0.7.15';

type

    { TForm_Main }

    TForm_Main = class(TForm)
        GroupBox1: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Lab_Type: TLabel;
        Lab_Topic: TLabel;
        Lab_Time: TLabel;
        MainMenu1: TMainMenu;
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
        SBar: TStatusBar;
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
        procedure StringGrid_MainSelection(Sender: TObject; aCol, aRow: Integer
            );
    private
        IsStartup: boolean;  //signalisiert ob gerade der Programmstart ausgeführt wird
        CurrentFile: string;  //Dateiname und Pfad der aktuell geöffneten Datei
        CfgFileName: string;  //Dateiname und Pfad der Konfigurationsdatei
        CfgINI: TIniFile;
        //Höhe und Breite von Form_Main, um sie nach Neustart wiederherzustellen
        myHeight, myWidth: integer;
        procedure ReadConfig();
        procedure SaveConfig();
        procedure OpenFile(Filename: string);

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
        OpenFile(CurrentFile);
    end;
end;

{******************************************************************************
Menü: Über KoCoLog
******************************************************************************}
procedure TForm_Main.MenIt_AboutClick(Sender: TObject);
begin
    MessageDlg('Über KoCoLog', MY_VERSION + LineEnding + '©2021-2023 Ingo Steiniger' +
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
        case StringGrid_Main.Cells[2, aRow] of
            'WARN': begin
                StringGrid_Main.canvas.Brush.Color := TColor($10CAEC);
            end;
            'ERR': begin
                StringGrid_Main.canvas.Brush.Color := TColor($A0A0FF);
            end;
            'FATAL': begin
                StringGrid_Main.canvas.Brush.Color := TColor($D050D0);
            end;
            else
            begin
                StringGrid_Main.Canvas.Font.Color := clBlack;
            end;
        end;

        StringGrid_Main.Canvas.FillRect(arect);
        StringGrid_Main.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, StringGrid_Main.Cells[aCol, aRow]);
        StringGrid_Main.Canvas.FrameRect(aRect);
    end;
end;

procedure TForm_Main.StringGrid_MainSelection(Sender: TObject; aCol,
    aRow: Integer);
begin
    Lab_Time.Caption:= StringGrid_Main.Cells[0, aRow];
    Lab_Topic.Caption:= StringGrid_Main.Cells[1, aRow];
    Lab_Type.Caption:= StringGrid_Main.Cells[2, aRow];
    Memo_Param.Text:= StringGrid_Main.Cells[3, aRow];
    aCol := aCol;
end;

{******************************************************************************
Prozedur: Konfigurationsdatei lesen
******************************************************************************}
procedure TForm_Main.ReadConfig();
begin
    cfgINI := TINIFile.Create(CfgFileName);

    //Dimension und Position der MainForm
    Top := Scale96ToScreen(cfgINI.ReadInteger('Window', 'Top', 100));
    Left := Scale96ToScreen(cfgINI.ReadInteger('Window', 'Left', 200));
    Width := Scale96ToForm(cfgINI.ReadInteger('Window', 'Width', 800));
    Height := Scale96ToForm(cfgINI.ReadInteger('Window', 'Height', 600));
    WindowState := TWindowState(cfgINI.ReadInteger('Window', 'State', 0));
    myHeight := Height;
    myWidth := Width;

    //Größe der Spalten des StringGrid
    StringGrid_Main.Columns.Items[0].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '0', 150));
    StringGrid_Main.Columns.Items[1].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '1', 300));
    StringGrid_Main.Columns.Items[2].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '2', 64));
    StringGrid_Main.Columns.Items[3].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '3', 1000));

    //Option auf-/absteigende Reihenfolge
    MenIt_Descending.Checked := cfgINI.ReadBool('Option', 'Asc', False);

    FreeAndNil(CfgINI);
end;

{******************************************************************************
Prozedur: Konfigurationsdatei speichern
******************************************************************************}
procedure TForm_Main.SaveConfig();
begin
    cfgINI := TINIFile.Create(cfgFileName);

    //Dimension und Position der MainForm
    if (WindowState = wsNormal) then //nicht speichern, wenn maximiert, minimiert
    begin
        cfgINI.WriteInteger('Window', 'Top', ScaleScreenTo96(Top));
        cfgINI.WriteInteger('Window', 'Left', ScaleScreenTo96(Left));
        cfgINI.WriteInteger('Window', 'Width', ScaleFormTo96(Width));
        cfgINI.WriteInteger('Window', 'Height', ScaleFormTo96(Height));
    end;
    cfgINI.WriteInteger('Window', 'State', Ord(WindowState));

    //Größe der Spalten des StringGrid
    cfgINI.WriteInteger('SG', '0', ScaleFormTo96(StringGrid_Main.Columns.Items[0].Width));
    cfgINI.WriteInteger('SG', '1', ScaleFormTo96(StringGrid_Main.Columns.Items[1].Width));
    cfgINI.WriteInteger('SG', '2', ScaleFormTo96(StringGrid_Main.Columns.Items[2].Width));
    cfgINI.WriteInteger('SG', '3', ScaleFormTo96(StringGrid_Main.Columns.Items[3].Width));


    //Option auf-/absteigende Reihenfolge
    cfgINI.WriteBool('Option', 'Asc', MenIt_Descending.Checked);

    FreeAndNil(cfgINI);
end;

{*******************************************************************************
Prozedur: Datei öffnen und Anzahl Einträge in der Statusbar anzeigen
Beschreibung: Vor- und Nachbereitung zum öffenen der Datei
Parameter: aFilename = vollständiger Dateiname mit Pfad
*******************************************************************************}
procedure TForm_Main.OpenFile(Filename: string);
begin
    if (FileExists(Filename)) then
    begin
        CurrentFile := Filename;
        Caption := MY_VERSION + ' - ' + Filename;
        Sbar.SimpleText := 'verarbeite Daten...';
        Application.ProcessMessages();

        if (ReadFile(Filename, StringGrid_Main, MenIt_Descending.Checked)) then
        begin
            StringGrid_MainSelection(nil, 0, 1);
            SBar.SimpleText := 'Anzahl Einträge: ' + Format('%.0N', [StringGrid_Main.RowCount / 1]);
            if (Form_Filter.IsFiltered) then
            begin
                SBar.SimpleText := SBar.SimpleText + '(gefiltert)';
            end;
        end else
        begin
            SBar.SimpleText := 'Fehler beim lesen der Datei "' + Filename + '"';
        end;
    end else
    begin
        if (Filename <> '') then
        begin
            MessageDlg('Fehler', 'Fehler: Die Datei "' + Filename + '" konnte nicht gefunden werden!' +
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
    IsStartup := True;
    CurrentFile := '';

    CfgFileName := GetAppConfigDir(False);
    ForceDirectories(CfgFileName); //sicherstellen, dass das Verzeichnis existiert
    CfgFileName += 'config.ini';
    ReadConfig();
end;

{*******************************************************************************
Ereignis: Programmstart #2
*******************************************************************************}
procedure TForm_Main.FormShow(Sender: TObject);
begin
    if (IsStartup) then
    begin  //Aufrufparameter auswerten
        if (ParamCount > 0) then
        begin
            Form_Filter.IsFiltered := False;
            //Datei mit OpenFile() öffnen, damit der Filter aufgerufen wird!
            OpenFile(ParamStr(1));
        end;
        isStartup := False;
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
        Height := myHeight;
        Width := myWidth;
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
    OpenFile(CurrentFile);
end;

end.
