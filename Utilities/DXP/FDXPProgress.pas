unit FDXPProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TDXPProgress = class(TForm)
    Panel1: TPanel;
    BUOk: TButton;
    Label1: TLabel;
    STProject: TStaticText;
    STStatus: TStaticText;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    LAErrors: TLabel;
    LAWarnings: TLabel;
    LAHints: TLabel;
    LANotes: TLabel;
    BUAbort: TButton;
    Panel6: TPanel;
    LATime: TLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure BUOkClick(Sender: TObject);
  private
    { Déclarations privées }
    FStartTime : TDateTime;
  public
    { Déclarations publiques }
    procedure SetProject(const projectName : String);
    procedure SetStatus(const status : String);
    procedure SetStat(const errors, warnings, hints, notes : Integer);
  end;

implementation

{$R *.dfm}

procedure TDXPProgress.SetProject(const projectName : String);
begin
   STProject.Caption:=' Project:   '+projectName;
end;

procedure TDXPProgress.SetStatus(const status : String);
begin
   STStatus.Caption:=status;
end;

procedure TDXPProgress.SetStat(const errors, warnings, hints, notes : Integer);
begin
   LAErrors.Caption:=IntToStr(errors)+'  ';
   LAWarnings.Caption:=IntToStr(warnings)+'  ';
   LAHints.Caption:=IntToStr(hints)+'  ';
   LANotes.Caption:=IntToStr(notes)+'  ';
end;

procedure TDXPProgress.FormCreate(Sender: TObject);
begin
   FStartTime:=Now;
   TimerTimer(Self);
end;

procedure TDXPProgress.TimerTimer(Sender: TObject);
begin
   LATime.Caption:=FormatDateTime('hh:nn:ss  ', Now-FStartTime);
end;

procedure TDXPProgress.BUOkClick(Sender: TObject);
begin
   Close;
end;

end.
