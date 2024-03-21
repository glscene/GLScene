unit fdGLSLshaders;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TFormGLSLshaders = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelGLSL: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGLSLshaders: TFormGLSLshaders;

implementation

{$R *.dfm}

end.
