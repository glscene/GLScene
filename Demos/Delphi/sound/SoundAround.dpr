{ 3D Sound sample (FMOD, BASS & OpenAL managers are used in this sample).

   This sample has a moving red sound source with a looping sound, and a "mickey"
   listener that you can move around using the trackbars.

   You already know the TGLScene, TGLSceneViewer, the TTimer is just used for
   regularly updating the Form's caption with FPS and CPU usage stats. You also
   know the TGLCadencer, but here, it not only cadenced the scene, but is also
   referred and used by the sound managers TGLSMFMOD, TGLSMBASS and TGLSMOpenAL.

   A TGLSoundLibrary is used to load and store the sample, a 44kHz WAV file. The
   sound library can be used to embed sound files in the application, you just
   have to add a sample at design-time and this removes the need for an external
   file, but for our samples, we share a single wav files among all demos.
   Sound libraries are used to store sound samples, you may only play a sample
   that is available in library (you can add/remove samples dynamically).

   We also have sound manager. There can only be one *active* sound manager in
   any app at any time, it serves as an interface to a low-level sound API.
   3D sounds are dynamic things, the easiest way to achieve this is to connect
   the manager to the cadencer by setting its "Cadencer" property, this way,
   it will get updated regularly.

   And now, the last part : a sound emitter behaviour has been attached to the
   red sphere, in this behaviour we specify a sample (by selecting a sound
   library, and a sample in the sound library). The "NbLoops" property was also
   adjusted, to keep our sound looping (playing again and again).

   That's basicly all you need to use GLScene Sound System. Note however, that
   depending on the low-level API you chose (ie. sound manager), some features
   amy or may not be available, but you don't need to worry about that, if
   a feature is unavailable on a particular driver, it will just be ignored.

   For the sake of the demo, all three samples are using different formats,
   the APIs take care of the conversions: "drumloop.wav" is a 44kHz PCM,
   "howl.mp3" a 16kHz MP3, and "chimes.wav" a 22kHz PCM... All three files
   however are mono, because stereo sounds cannot go 3D... Remember that only
   3D sounds are required to be mono, if you have some background music or ambient
   soundtrack, it can be stereo (use the BASS or FMOD API directly to play it).

   The OpenAL manager currently accepts only simple *uncompressed* WAV files
   (8/16 bits, mono/stereo), so you will need to convert files to these format
   before using it.
}
program SoundAround;



uses
  Forms,
  SoundAroundFm in 'SoundAroundFm.pas' {FormSoundAround};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSoundAround, FormSoundAround);
  Application.Run;
end.
