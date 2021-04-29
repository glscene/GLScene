{: Basic viewer for HTF Content.

   Gives basic time stats for HTF data extraction and rendering (there is NO
   cache, each tile is reloaded each time from the disk, ie. those are the
   timings you could expect when accessing an HTF area for the first time or
   when "moving at high speed").

   Requires the Graphics32 library (http://www.g32.org).
}
program HTFViewer;

uses
  Forms,
  FViewerForm in 'FViewerForm.pas' {ViewerForm},
  FNavForm in 'FNavForm.pas' {NavForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewerForm, ViewerForm);
  Application.CreateForm(TNavForm, NavForm);
  Application.Run;
end.
