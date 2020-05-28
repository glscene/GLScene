GLSL Validation Expert for Delphi
Copyright 2005 - Eric Grange / GLScene.org

Intended to work along with 3DLabs GLSL Validator

	http://developer.3dlabs.com/openGL2/downloads/

(install at default location in C:\Program Files)

This expert takes over Ctrl+Shift+V combination in the IDE
and invokes the GLSL Validator. The file name that is being 
validated must end in '_vp.glsl' for a vertex program, 
and '_fp.glsl' for a fragment program. Errors if any are
added in the tools/compiler report area, and you can then
double-click them to jump to the error location in the source.