unit klfilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, DateUtils;

type

  { TForm_Filter }
  TForm_Filter = class(TForm)
    Btn_Filter: TButton;
    Btn_Cancel: TButton;
    ChBo_CaseSensitive: TCheckBox;
    ChBo_Timespan: TCheckBox;
    CoBo_AndOr1: TComboBox;
    CoBo_AndOr2: TComboBox;
    CoBo_Col2: TComboBox;
    CoBo_Col3: TComboBox;
    CoBo_Operator1: TComboBox;
    CoBo_Col: TComboBox;
    CoBo_Operator: TComboBox;
    CoBo_Col1: TComboBox;
    CoBo_AndOr: TComboBox;
    CoBo_Operator2: TComboBox;
    CoBo_Operator3: TComboBox;
    Date_From: TDateEdit;
    Date_To: TDateEdit;
    Edit_Filter: TEdit;
    Edit_Filter1: TEdit;
    Edit_Filter2: TEdit;
    Edit_Filter3: TEdit;
    GrBo_Timespan: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Time_From: TTimeEdit;
    Time_To: TTimeEdit;
    procedure Btn_FilterClick(Sender: TObject);
    procedure Btn_CancelClick(Sender: TObject);
    procedure ChBo_TimespanChange(Sender: TObject);
    procedure CoBo_AndOrChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure EnableControls();
    function ValidateForm(): boolean;
    function HasValidDateTimes(): boolean;

  public
    IsFiltered: boolean;  //signalisiert ob die Einträge gefiltert werden oder nicht
    function MatchFilter(aText, aCol: string; DoesMatch: boolean): boolean;
    function MatchAllFilters(Input: TStrings): boolean;

  end;


var
  Form_Filter: TForm_Filter;

implementation

{$R *.lfm}
{******************************************************************************
Ereignis: Button "Filtern" gedrückt
******************************************************************************}
procedure TForm_Filter.Btn_FilterClick(Sender: TObject);
begin
  //Wenn CoBo_Col gewählt und Edit_Filter ausgefüllt
  if (ValidateForm()) then
  begin
    ModalResult := mrOk; //Fenster schließen
  end
  else  //Ansonsten Fehlermeldung
  begin
    MessageDlg('Fehler',
      'Bitte wählen Sie eine Spalte aus und geben Sie einen Text ein oder wählen Sie Zeitraum und geben einen gültigen Zeitraum ein, um zu filtern.' + LineEnding + 'Oder drücken Sie "Abbrechen", um den Filter zu löschen.',
      mtError, [mbOK], 0);
  end;
end;

{******************************************************************************
Ereignis: Button "Abbruch" gedrückt
******************************************************************************}
procedure TForm_Filter.Btn_CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TForm_Filter.ChBo_TimespanChange(Sender: TObject);
begin
  GrBo_Timespan.Enabled := ChBo_Timespan.Checked;
end;

{******************************************************************************
Ereignis: Änderung der Auswahl
******************************************************************************}
procedure TForm_Filter.CoBo_AndOrChange(Sender: TObject);
begin
  EnableControls();
end;

{******************************************************************************
Ereignis: Anzeige der Form
******************************************************************************}
procedure TForm_Filter.FormShow(Sender: TObject);
begin
  SelectFirst();
  EnableControls();
end;

{******************************************************************************
Prozedur: enableControls
Beschreibung: dis-/enabled die Controls, abhängig von CoBo_AndOr.
              Wird von FormShow und CoBo_AndOrChange aufgerufen
******************************************************************************}
procedure TForm_Filter.EnableControls();
var
  doEnable: boolean;
begin
  doEnable := CoBo_AndOr.ItemIndex > 0;
  CoBo_Col1.Enabled := doEnable;
  CoBo_Operator1.Enabled := doEnable;
  Edit_Filter1.Enabled := doEnable;
  CoBo_AndOr1.Enabled := doEnable;

  doEnable := doEnable and (CoBo_AndOr1.ItemIndex > 0);
  CoBo_Col2.Enabled := doEnable;
  CoBo_Operator2.Enabled := doEnable;
  Edit_Filter2.Enabled := doEnable;
  CoBo_AndOr2.Enabled := doEnable;

  doEnable := doEnable and (CoBo_AndOr2.ItemIndex > 0);
  CoBo_Col3.Enabled := doEnable;
  CoBo_Operator3.Enabled := doEnable;
  Edit_Filter3.Enabled := doEnable;
end;

{******************************************************************************
Funktion: validateForm
Beschreibung: Prüft ob Form_Filter korrekt ausgefüllt ist
Rückgabe: True - die Controls sind befüllt
          False - min. ein Control ist nicht gefüllt
******************************************************************************}
function TForm_Filter.ValidateForm(): boolean;
begin
  Result := ((CoBo_Col.ItemIndex >= 0) and (Edit_Filter.Text <> '')) or
    (ChBo_Timespan.Checked and HasValidDateTimes);

  if (CoBo_AndOr.ItemIndex > 0) then
  begin
    Result := Result and (CoBo_Col1.ItemIndex >= 0) and (Edit_Filter1.Text <> '');
  end;
  if (CoBo_AndOr1.ItemIndex > 0) then
  begin
    Result := Result and (CoBo_Col2.ItemIndex >= 0) and (Edit_Filter2.Text <> '');
  end;
  if (CoBo_AndOr2.ItemIndex > 0) then
  begin
    Result := Result and (CoBo_Col3.ItemIndex >= 0) and (Edit_Filter3.Text <> '');
  end;
end;

function TForm_Filter.HasValidDateTimes: boolean;
var
  t: TDateTime;
begin
  Result := TryStrToDateTime(Date_From.Text + ' ' + Time_From.Text, t);
  Result := Result and TryStrToDateTime(Date_To.Text + ' ' + Time_To.Text, t);
end;

{******************************************************************************
Funktion: Filter
Beschreibung: Prüft ob die übergebene Zeile den Filterkriterien enspricht
Parameter: Input - eine StringList mit den 4 Zeilen für die 4 Spalten
Rückgabe: True - entspricht dem Filter
          False - entspricht nicht dem Filter
******************************************************************************}
function TForm_Filter.MatchFilter(aText, aCol: string; DoesMatch: boolean): boolean;
begin
  //Groß-/Kleinschreibung beachten?
  if (ChBo_CaseSensitive.Checked) then
  begin
    Result := Pos(aText, aCol) > 0;
  end
  else
  begin
    Result := Pos(LowerCase(aText), LowerCase(aCol)) > 0;
  end;

  //ggf. Ergebnis invertieren
  if (not DoesMatch) then
  begin
    Result := not Result;
  end;
end;

{******************************************************************************
Funktion: alle Filter prüfen
Beschreibung: Prüft ob die übergebene Zeile allen Filterkriterien enspricht
Parameter: Input - eine StringList mit den 4 Zeilen für die 4 Spalten
Rückgabe: True - entspricht dem Filtern
          False - entspricht nicht dem Filtern
******************************************************************************}
function TForm_Filter.MatchAllFilters(Input: TStrings): boolean;
var
  aText, aCol, CurrDateString: string;
  aMatch: boolean;
  FromDate, ToDate, CurrDate: TDateTime;
begin
  Result := True;
  //erster Filter
  if (CoBo_Col.ItemIndex >= 0) then
  begin
    aCol := Input[CoBo_Col.ItemIndex];
    aMatch := CoBo_Operator.ItemIndex = 0;
    //"enthält" = True/"enthät nicht" = False
    aText := Edit_Filter.Text;

    Result := matchFilter(aText, aCol, aMatch);

    //zweiter Filter
    if (CoBo_AndOr.ItemIndex > 0) then
    begin
      case CoBo_AndOr.ItemIndex of
        1: begin
          aCol := Input[CoBo_Col1.ItemIndex];
          aMatch := CoBo_Operator1.ItemIndex = 0;
          aText := Edit_Filter1.Text;
          Result := Result and MatchFilter(aText, aCol, aMatch);
        end;
        2: begin
          aCol := Input[CoBo_Col1.ItemIndex];
          aMatch := CoBo_Operator1.ItemIndex = 0;
          aText := Edit_Filter1.Text;
          Result := Result or MatchFilter(aText, aCol, aMatch);
        end;
        else
        begin
          //do nothing
        end;
      end;

      //dritter Filter
      if (CoBo_AndOr1.ItemIndex > 0) then
      begin
        case CoBo_AndOr1.ItemIndex of
          1: begin
            aCol := Input[CoBo_Col2.ItemIndex];
            aMatch := CoBo_Operator2.ItemIndex = 0;
            aText := Edit_Filter2.Text;
            Result := Result and MatchFilter(aText, aCol, aMatch);
          end;
          2: begin
            aCol := Input[CoBo_Col2.ItemIndex];
            aMatch := CoBo_Operator2.ItemIndex = 0;
            aText := Edit_Filter2.Text;
            Result := Result or MatchFilter(aText, aCol, aMatch);
          end;
          else
          begin
            //do nothing
          end;
        end;

        //vierter Filter
        if (CoBo_AndOr2.ItemIndex > 0) then
        begin
          case CoBo_AndOr2.ItemIndex of
            1: begin
              aCol := Input[CoBo_Col3.ItemIndex];
              aMatch := CoBo_Operator3.ItemIndex = 0;
              aText := Edit_Filter3.Text;
              Result := Result and MatchFilter(aText, aCol, aMatch);
            end;
            2: begin
              aCol := Input[CoBo_Col3.ItemIndex];
              aMatch := CoBo_Operator3.ItemIndex = 0;
              aText := Edit_Filter3.Text;
              Result := Result or MatchFilter(aText, aCol, aMatch);
            end;
            else
            begin
              //do nothing
            end;
          end;
        end;
      end;
    end;
  end;

  //Zeitraum
  if (ChBo_Timespan.Checked) then
  begin
    FromDate := StrToDateTime(Date_From.Text + ' ' + Time_From.Text);
    ToDate := StrToDateTime(Date_To.Text + ' ' + Time_To.Text);
    CurrDateString := Copy(Input[0], 1, 16);
    CurrDate := StrToDateTime(CurrDateString);

    Result := Result and (FromDate <= CurrDate) and (ToDate >= CurrDate);
  end;
end;

end.
