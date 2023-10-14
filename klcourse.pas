unit klcourse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, DateUtils;

const
  MINUTES_PER_WEEK = 10080;
  MINUTES_PER_DAY = 1440;
  OFFSET_X = 20;

type

  { TForm_Course }

  TForm_Course = class(TForm)
    PB_Course: TPaintBox;
    procedure FormShow(Sender: TObject);
    procedure PB_CoursePaint(Sender: TObject);
  private
    LastDay, FirstDay: TDateTime;
    PixelPerMinute: double;
    procedure PaintBaseLine();
    procedure PaintDayLines();
    procedure PaintWeekLines();
    procedure PaintScale();
    procedure PaintStartAndEndDate();
    procedure PaintMessages();
    procedure PaintMessage(DateTimeInMinutes: int64; MessageType: string);
    function GetNextMonday(NumberOfWeek: integer): TDateTime;
    function GetDateFromRow(RowNumber: integer): TDateTime;
    function StringGridNotEmpty(): boolean;
  public
    theStringGrid: TStringGrid;
  end;

var
  Form_Course: TForm_Course;

implementation

{$R *.lfm}

{ TForm_Course }

procedure TForm_Course.PB_CoursePaint(Sender: TObject);
begin
  PB_Course.Canvas.Brush.Color := clWhite;
  PB_Course.Canvas.FillRect(0, 0, PB_Course.Width, PB_Course.Height);

  if (StringGridNotEmpty()) then
  begin
    LastDay := GetDateFromRow(1);
    FirstDay := GetDateFromRow(theStringGrid.RowCount - 1);
    PixelPerMinute := (PB_Course.Width - 2 * OFFSET_X) /
      MinutesBetween(FirstDay, LastDay);

    PaintScale();
    PaintMessages();
  end;
end;

{******************************************************************************
Prozedur: PaintScale
Beschreibung: Zeichnet die Scala(Tage/Wochen)
******************************************************************************}
procedure TForm_Course.PaintScale();
begin
  PaintBaseLine();
  PaintStartAndEndDate();
  PaintDayLines();
  PaintWeekLines();
end;

{******************************************************************************
Prozedur: PaintMessages
Beschreibung: Durchläuft das Stringrid und lässt die Nechrichten einzeichnen.
******************************************************************************}
procedure TForm_Course.PaintMessages();
var
  i: integer;
  DateTimeInMinutes: int64;
  tmpDateTime: TDateTime;
  MessageType: string;
begin

  for i := 1 to theStringGrid.RowCount - 1 do
  begin
    MessageType := UpperCase(theStringGrid.Cells[2, i]);

    if (MessageType <> 'INFO') then
    begin
      tmpDateTime := GetDateFromRow(i);
      DateTimeInMinutes := MinutesBetween(FirstDay, tmpDateTime);
      PaintMessage(DateTimeInMinutes, MessageType);
    end;
  end;
end;

{******************************************************************************
Prozedur: PaintValue
Beschreibung: Zeichnet einen Wert in die PaintPox.
******************************************************************************}
procedure TForm_Course.PaintMessage(DateTimeInMinutes: int64; MessageType: string);
var
  x, y1, y2: integer;
  yOffset: integer;
begin

  case MessageType of
    'WARN':
    begin
      PB_Course.Canvas.Pen.Color := TColor($10CAEC);
      yOffset := 100;
    end;
    'ERR':
    begin
      PB_Course.Canvas.Pen.Color := TColor($A0A0FF);
      yOffset := 80;
    end;
    'FATAL':
    begin
      PB_Course.Canvas.Pen.Color := TColor($D050D0);
      yOffset := 60;
    end;
    else
    begin
      PB_Course.Canvas.Pen.Color := clBlack;
      yOffset := 120;
    end;
  end;

  x := Round(DateTimeInMinutes * PixelPerMinute) + OFFSET_X;
  y1 := PB_Course.Height - yOffset;
  y2 := PB_Course.Height - yOffset - 10;

  PB_Course.Canvas.Line(x, y1, x, y2);
end;

function TForm_Course.GetNextMonday(NumberOfWeek: integer): TDateTime;
begin
  Result := EndOfTheWeek(FirstDay) + 1 + (NumberOfWeek * 7);
end;

//Tage
procedure TForm_Course.PaintDayLines();
var
  StartDay, MinutesSinceFirstMessage: int64;
  DaysCount, i: integer;
  x, y1, y2: integer;
begin
  StartDay := MinutesBetween(FirstDay, EndOfTheDay(FirstDay));
  DaysCount := DaysBetween(FirstDay, LastDay);

  PB_Course.Canvas.Pen.Color := clSilver;

  for i := 0 to DaysCount - 1 do
  begin
    MinutesSinceFirstMessage := int64(i) * MINUTES_PER_DAY + StartDay;

    x := Round(MinutesSinceFirstMessage * PixelPerMinute) + OFFSET_X;
    y1 := PB_Course.Height - 41;
    y2 := PB_Course.Height - 120;

    PB_Course.Canvas.Line(x, y1, x, y2);
  end;
end;

//Basislinie
procedure TForm_Course.PaintBaseLine();
var
  x1, x2, y: integer;
begin
  x1 := OFFSET_X;
  x2 := PB_Course.Width - OFFSET_X;
  y := PB_Course.Height - 40;

  PB_Course.Canvas.Pen.Color := clBlack;
  PB_Course.Canvas.Line(x1, y, x2, y);
end;

function TForm_Course.GetDateFromRow(RowNumber: integer): TDateTime;
begin
  Result := StrToDateTime(copy(theStringGrid.Cells[0, RowNumber], 1, 19));
end;

function TForm_Course.StringGridNotEmpty(): boolean;
begin
  Result := theStringGrid.RowCount > 1;
end;

//Beschriftung unten
procedure TForm_Course.PaintStartAndEndDate();
var
  x, y: integer;
begin
  x := OFFSET_X;
  y := PB_Course.Height - 40;
  PB_Course.Canvas.Line(x, y, x, y + 10);
  PB_Course.Canvas.TextOut(x, y + 15, DateToStr(FirstDay));

  x := PB_Course.Width - OFFSET_X;
  PB_Course.Canvas.Line(x, y, x, y + 10);
  PB_Course.Canvas.TextOut(x - 50, y + 15, DateToStr(LastDay));
end;

//Wochen
procedure TForm_Course.PaintWeekLines();
var
  MinutesOfFirstWeek, MinutesSinceFirstMessage: int64;
  WeeksCount, i: integer;
  x, y1, y2: integer;
begin
  MinutesOfFirstWeek := MinutesBetween(FirstDay, EndOfTheWeek(FirstDay));
  WeeksCount := WeeksBetween(FirstDay, LastDay);
  PB_Course.Canvas.Pen.Color := clGray;

  for i := 0 to WeeksCount do
  begin
    MinutesSinceFirstMessage := int64(i) * MINUTES_PER_WEEK + MinutesOfFirstWeek;

    x := Round(MinutesSinceFirstMessage * PixelPerMinute) + OFFSET_X;
    y1 := PB_Course.Height - 41;
    y2 := PB_Course.Height - 140;

    PB_Course.Canvas.Line(x, y1, x, y2);
    PB_Course.Canvas.TextOut(x - 19, y2 - 20, DateToStr(GetNextMonday(i)));
  end;
end;

procedure TForm_Course.FormShow(Sender: TObject);
begin
  PB_Course.Invalidate();
end;

end.
