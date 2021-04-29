{: Utility to pack one or many DEM sources into a single HTF.

   Note: this is a *basic* tool, error messages are unfriendly and there are
         memory leaks if you do any, I know. So, don't do errors ;)

         DTED data is organized in columns, instead of rows, like most other DEM's.
         This means that an inported DTED file will have to be rotated by 270 degrees,
         if you want north at the top. DTED level 2 maps will have a size of 1801x3601

  10/04/2007 -LIN- Added support for DTED DEM files.
                   Added Flip/Rotate. (Use Rotate 270 for DTED files.)
}
program TerrainPack;

uses
  Forms,
  FMainForm in 'FMainForm.pas' {MainForm},
  FNavForm in 'FNavForm.pas' {NavForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TNavForm, NavForm);
  Application.Run;
end.
