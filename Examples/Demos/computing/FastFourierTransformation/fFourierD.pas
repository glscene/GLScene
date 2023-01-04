unit fFourierD;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, CUDA.Compiler, CUDA.Context,
  CUDA.APIComps;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label4: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    GLCUDA1: TGLCUDA;
    GLCUDADevice1: TGLCUDADevice;
    GLCUDACompiler1: TGLCUDACompiler;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

end.
