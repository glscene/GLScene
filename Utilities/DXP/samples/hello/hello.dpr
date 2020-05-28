program hello;

{$APPTYPE CONSOLE}

uses VectorGeometry, OpenGL1x;

var
   vr : TAffineVector;
begin
   Writeln('Hello World!');
   Writeln('');
   Writeln('XVector + ZVector = ');
   vr:=VectorAdd(XVector, ZVector);
   Writeln(vr[0], vr[1], vr[2]);
   Writeln('');
   LoadOpenGL;
   if IsOpenGLLoaded then
      Writeln('Found GLU: '+gluGetString(GLU_VERSION))
   else Writeln('GLU not found.');
   ReadLn;
end.
