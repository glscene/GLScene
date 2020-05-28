{
  Simple spherical panorama viewer using GLScene

  The sample input images are by Philippe Hurbain. http://www.philohome.com

  Other resources on how to make your own spherical or cylindrical panorama:
    http://www.fh-furtwangen.de/~dersch/
    http://www.panoguide.com/
    http://home.no.net/dmaurer/~dersch/Index.htm

  Why IPIX patents regarding use of fisheye photos are questionable:
    http://www.worldserver.com/turk/quicktimevr/fisheye.html

  10/12/02 - EG - Updated for GLScene v0.9+
}
program PanoViewer;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
