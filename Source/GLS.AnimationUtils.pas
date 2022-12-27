//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.AnimationUtils;

(* Main purpose is to give an easy way to create an interpolation. *)

interface

{$I GLScene.inc}

uses
  System.SysUtils, 
  System.Classes, 
  System.Math,
  GLS.VectorTypes,
  GLS.VectorGeometry;

type

  TEaseType=
  (
    etLinear,
    etQuadIn,
    etQuadOut,
    etQuadInOut,
    etQuadOutIn,
    etCubicIn,
    etCubicOut,
    etCubicInOut,
    etCubicOutIn,
    etQuintIn,
    etQuintOut,
    etQuintInOut,
    etQuintOutIn,
    etSineIn,
    etSineOut,
    etSineInOut,
    etSineOutIn,
    etCircIn,
    etCircOut,
    etCircInOut,
    etCircOutIn,
    etExpoIn,
    etExpoOut,
    etExpoInOut,
    etExpoOutIn,
    etElasticIn,
    etElasticOut,
    etElasticInOut,
    etElasticOutIn,
    etBackIn,
    etBackOut,
    etBackInOut,
    etBackOutIn,
    etBounceIn,
    etBounceOut,
    etBounceInOut,
    etBounceOutIn
  );


  function Tweener(Current, Target: TAffineVector; Time, Duration: Single; EaseType: TEaseType): TAffineVector; overload;
  function Tweener(Current, Target: TGLVector; Time, Duration: Single; EaseType: TEaseType): TGLVector; overload;
  function Tweener(Current, Target: TVector2f; Time, Duration: Single; EaseType: TEaseType): TVector2f; overload;
  function Tweener(Current, Target: Single; Time, Duration: Single; EaseType: TEaseType): Single; overload;

//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------

type

  TEaseFunction = function(t:Single; b:Single; c:Single; d:Single):Single;


(*
  Easing equation function for a simple linear tweening, with no easing.

  t		Current time (in frames or seconds).
  b		Starting value.
  c		Change needed in value.
  d		Expected easing duration (in frames or seconds).
  Result		The correct value.
 *)
function easeNone (t:Single; b:Single; c:Single; d:Single):Single;
begin
  Result := c*t/d + b;
end;

(*
  Easing equation function for a quadratic (t^2) easing in: accelerating from zero velocity.

  t		Current time (in frames or seconds).
  b		Starting value.
  c		Change needed in value.
  d		Expected easing duration (in frames or seconds).
  Result		The correct value.
 *)
function easeInQuad (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  Result := c*t*t + b;
end;

(*
  Easing equation function for a quadratic (t^2) easing out: decelerating to zero velocity.
 
  t		Current time (in frames or seconds).
  b		Starting value.
  c		Change needed in value.
  d		Expected easing duration (in frames or seconds).
  Result		The correct value.
 *)
function easeOutQuad (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  Result := -c *t*(t-2) + b;
end;

(*
  Easing equation function for a quadratic (t^2) easing in/out: acceleration until halfway, then deceleration.
 
  t		Current time (in frames or seconds).
  b		Starting value.
  c		Change needed in value.
  d		Expected easing duration (in frames or seconds).
  Result		The correct value.
 *)
function easeInOutQuad (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/(d/2);
  if (t < 1) then
    Result := c/2*t*t + b
  else
  begin
    t := t-1;
    Result := -c/2 * (t*(t-2) - 1) + b;
  end;
end;

(*
  Easing equation function for a quadratic (t^2) easing out/in: deceleration until halfway, then acceleration.
 
  t		Current time (in frames or seconds).
  b		Starting value.
  c		Change needed in value.
  d		Expected easing duration (in frames or seconds).
  Result		The correct value.
 *)
function easeOutInQuad (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutQuad (t*2, b, c/2, d)
  else
    Result := easeInQuad((t*2)-d, b+c/2, c/2, d);
end;

(*
   Easing equation function for a cubic (t^3) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInCubic (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  Result := c*t*t*t + b;
end;

(*
  Easing equation function for a cubic (t^3) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutCubic (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d-1;
  Result := c*(t*t*t + 1) + b;
end;

(*
  Easing equation function for a cubic (t^3) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInOutCubic (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/(d/2);
  if (t < 1) then
    Result := c/2*t*t*t + b
  else
  begin
    t := t-2;
    Result := c/2*(t*t*t + 2) + b;
  end;
end;

(*
  Easing equation function for a cubic (t^3) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInCubic (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutCubic (t*2, b, c/2, d)
  else
    Result := easeInCubic((t*2)-d, b+c/2, c/2, d);
end;

(*
  Easing equation function for a quartic (t^4) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInQuart (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  Result := c*t*t*t*t + b;
end;

(*
  Easing equation function for a quartic (t^4) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutQuart (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d-1;
  Result := -c * (t*t*t*t - 1) + b;
end;

(*
  Easing equation function for a quartic (t^4) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInOutQuart (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/(d/2);
  if (t < 1) then
    Result := c/2*t*t*t*t + b
  else
  begin
    t := t-2;
    Result := -c/2 * (t*t*t*t - 2) + b;
  end;
end;

(*
  Easing equation function for a quartic (t^4) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInQuart (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutQuart (t*2, b, c/2, d)
  else
    Result := easeInQuart((t*2)-d, b+c/2, c/2, d);
end;

(*
  Easing equation function for a quintic (t^5) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInQuint (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  Result := c*t*t*t*t*t + b;
end;

(*
   Easing equation function for a quintic (t^5) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutQuint (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d-1;
  Result := c*(t*t*t*t*t + 1) + b;
end;

(*
  Easing equation function for a quintic (t^5) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInOutQuint (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/(d/2);
  if (t < 1) then
    Result := c/2*t*t*t*t*t + b
  else
  begin
    t := t-2;
    Result := c/2*(t*t*t*t*t + 2) + b;
  end;
end;

(*
   Easing equation function for a quintic (t^5) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInQuint (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutQuint (t*2, b, c/2, d)
  else
    Result := easeInQuint((t*2)-d, b+c/2, c/2, d);
end;

(*
   Easing equation function for a sinusoidal (sin(t)) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInSine (t:Single; b:Single; c:Single; d:Single):Single;
begin
  Result := -c * cos(t/d * (PI/2)) + c + b;
end;

(*
   Easing equation function for a sinusoidal (sin(t)) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutSine (t:Single; b:Single; c:Single; d:Single):Single;
begin
  Result := c * sin(t/d * (PI/2)) + b;
end;

(*
   Easing equation function for a sinusoidal (sin(t)) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInOutSine (t:Single; b:Single; c:Single; d:Single):Single;
begin
  Result := -c/2 * (cos(PI*t/d) - 1) + b;
end;

(*
   Easing equation function for a sinusoidal (sin(t)) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInSine (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutSine (t*2, b, c/2, d)
  else
    Result := easeInSine((t*2)-d, b+c/2, c/2, d);
end;

(*
   Easing equation function for an exponential (2^t) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInExpo (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if t=0 then
    Result := b
  else
    Result := c * Power(2, 10 * (t/d - 1)) + b - c * 0.001;
end;

(*
   Easing equation function for an exponential (2^t) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutExpo (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if t=d then
    Result := b+c
  else
    Result := c * 1.001 * (-Power(2, -10 * t/d) + 1) + b;
end;

(*
   Easing equation function for an exponential (2^t) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInOutExpo (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t=0) then
    Result := b
  else
  if (t=d) then
    Result := b+c
  else
  begin
    t := t/(d/2);
    if (t < 1) then
      Result := c/2 * Power(2, 10 * (t - 1)) + b - c * 0.0005
    else
    begin
      t := t-1;
      Result := c/2 * 1.0005 * (-Power(2, -10 * t) + 2) + b;
    end;
  end;
end;

(*
   Easing equation function for an exponential (2^t) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInExpo (t:Single; b:Single; c:Single; d:Single):Single; begin
  if (t < d/2) then
    Result := easeOutExpo (t*2, b, c/2, d)
  else
    Result := easeInExpo((t*2)-d, b+c/2, c/2, d);
end;

(*
   Easing equation function for a circular (sqrt(1-t^2)) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInCirc (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  Result := -c * (Sqrt(1 - t*t) - 1) + b;
end;

(*
   Easing equation function for a circular (sqrt(1-t^2)) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutCirc (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d-1;
  Result := c * Sqrt(1 - t*t) + b;
end;

(*
   Easing equation function for a circular (sqrt(1-t^2)) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeInOutCirc (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/(d/2);
  if (t < 1) then
    Result := -c/2 * (Sqrt(1 - t*t) - 1) + b
  else
  begin
    t := t-2;
    Result := c/2 * (Sqrt(1 - t*t) + 1) + b;
  end;
end;

(*
   Easing equation function for a circular (sqrt(1-t^2)) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInCirc (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutCirc (t*2, b, c/2, d)
  else
    Result := easeInCirc((t*2)-d, b+c/2, c/2, d);
end;

(*
   Easing equation function for an elastic (exponentially decaying sine wave) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   a		Amplitude.
   p		Period.
   Result		The correct value.
 *)
function easeInElastic (t:Single; b:Single; c:Single; d:Single):Single;
var
  p, a, s: Single;
begin
  if t=0 then
    Result := b
  else
  begin
    t := t/d;
    if t=1 then
      Result := b+c
    else
    begin
      p := d*0.3;
      a := c;
      s := p/4;
      t := t-1;
      Result := -((a * Power(2, 10*t)) * sin((t*d-s)*(2*PI)/p )) + b;
    end;
  end;
end;



(*
   Easing equation function for an elastic (exponentially decaying sine wave) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   a		Amplitude.
   p		Period.
   Result		The correct value.
 *)
function easeOutElastic (t:Single; b:Single; c:Single; d:Single):Single;
var
  p, a, s: Single;
begin
  if t=0 then
    Result := b
  else
  begin
    t := t/d;
    if t=1 then
      Result := b+c
    else
    begin
      p := d*0.3;
      a := c;
      s := p/4;
      Result :=  (a* Power(2,-10*t) * sin( (t*d-s)*(2*PI)/p ) + c + b);
    end;
  end;
end;

(*
   Easing equation function for an elastic (exponentially decaying sine wave) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   a		Amplitude.
   p		Period.
   Result		The correct value.
 *)
function easeInOutElastic (t:Single; b:Single; c:Single; d:Single):Single;
var
  p, a, s: Single;
begin
  if t=0 then
    Result := b
  else
  begin
    t := t/(d/2);
    if t=2 then
      Result := b+c
    else
    begin
      p := d*0.3*1.5;
      a := c;
      s := p/4;
      t := t-1;

      if t<1 then
        Result := -0.5* ((a* Power(2,10*t)) * sin( (t*d-s)*(2*PI)/p)) + b
      else
        Result := (a* Power(2,-10*t)) * sin( (t*d-s)*(2*PI)/p )*0.5 + c + b;
    end;
  end;
end;



(*
   Easing equation function for an elastic (exponentially decaying sine wave) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   a		Amplitude.
   p		Period.
   Result		The correct value.
 *)
function easeOutInElastic (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutElastic (t*2, b, c/2, d)
  else
    Result := easeInElastic((t*2)-d, b+c/2, c/2, d)
end;

(*
   Easing equation function for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   s		Overshoot ammount: higher s means greater overshoot (0 produces cubic easing with no overshoot, and the default value of 1.70158 produces an overshoot of 10 percent).
   Result		The correct value.
 *)
function easeInBack (t:Single; b:Single; c:Single; d:Single):Single;
var
  s: Single;
begin
  s := 1.70158;
  t := t/d;
  Result := c*t*t*((s+1)*t - s) + b;
end;

(*
   Easing equation function for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   s		Overshoot ammount: higher s means greater overshoot (0 produces cubic easing with no overshoot, and the default value of 1.70158 produces an overshoot of 10 percent).
   Result		The correct value.
 *)
function easeOutBack (t:Single; b:Single; c:Single; d:Single):Single;
var
  s: Single;
begin
  s := 1.70158;
  t := t/d-1;
  Result := c*(t*t*((s+1)*t + s) + 1) + b;
end;

(*
   Easing equation function for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   s		Overshoot ammount: higher s means greater overshoot (0 produces cubic easing with no overshoot, and the default value of 1.70158 produces an overshoot of 10 percent).
   Result		The correct value.
 *)
function easeInOutBack (t:Single; b:Single; c:Single; d:Single):Single;
var
  s: Single;
begin
  s := 1.70158;
  t := t/(d/2);
  s := s*1.525;
  if (t < 1) then
    Result := c/2*(t*t*((s+1)*t - s)) + b
  else
  begin
    t := t-2;
    Result := c/2*(t*t*((s+1)*t + s) + 2) + b;
  end;
end;

(*
   Easing equation function for a back (overshooting cubic easing: (s+1)*t^3 - s*t^2) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   s		Overshoot ammount: higher s means greater overshoot (0 produces cubic easing with no overshoot, and the default value of 1.70158 produces an overshoot of 10 percent).
   Result		The correct value.
 *)
function easeOutInBack (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutBack (t*2, b, c/2, d)
  else
    Result := easeInBack((t*2)-d, b+c/2, c/2, d);
end;



(*
   Easing equation function for a bounce (exponentially decaying parabolic bounce) easing out: decelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutBounce (t:Single; b:Single; c:Single; d:Single):Single;
begin
  t := t/d;
  if t < (1/2.75) then
    Result := c*(7.5625*t*t) + b
    else
  if t < (2/2.75) then
  begin
    t := t-(1.5/2.75);
    Result := c*(7.5625*t*t + 0.75) + b;
  end
    else
  if (t < (2.5/2.75)) then
  begin
    t := t-(2.25/2.75);
    Result := c*(7.5625*t*t + 0.9375) + b;
  end
    else
  begin
    t := t-(2.625/2.75);
    Result := c*(7.5625*t*t + 0.984375) + b;
  end;
end;

(*
   Easing equation function for a bounce (exponentially decaying parabolic bounce) easing in: accelerating from zero velocity.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
 * Result		The correct value.
 *)
function easeInBounce (t:Single; b:Single; c:Single; d:Single):Single;
begin
  Result := c - easeOutBounce (d-t, 0, c, d) + b;
end;

(*
   Easing equation function for a bounce (exponentially decaying parabolic bounce) easing in/out: acceleration until halfway, then deceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
 * Result		The correct value.
 *)
function easeInOutBounce (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeInBounce(t*2, 0, c, d) * 0.5 + b
  else
    Result := easeOutBounce(t*2-d, 0, c, d) * 0.5 + c*0.5 + b;
end;

(*
   Easing equation function for a bounce (exponentially decaying parabolic bounce) easing out/in: deceleration until halfway, then acceleration.
 
   t		Current time (in frames or seconds).
   b		Starting value.
   c		Change needed in value.
   d		Expected easing duration (in frames or seconds).
   Result		The correct value.
 *)
function easeOutInBounce (t:Single; b:Single; c:Single; d:Single):Single;
begin
  if (t < d/2) then
    Result := easeOutBounce(t*2, b, c/2, d)
  else
    Result := easeInBounce((t*2)-d, b+c/2, c/2, d);
end;

function GetEaseFunctionFromType(const EaseType: TEaseType): TEaseFunction;
begin
  case EaseType of
    etLinear       :   Result := easeNone;
    etQuadIn       :   Result := easeInQuad;
    etQuadOut      :   Result := easeOutQuad;
    etQuadInOut    :   Result := easeInOutQuad;
    etQuadOutIn    :   Result := easeOutInQuad;
    etCubicIn      :   Result := easeInCubic;
    etCubicOut     :   Result := easeOutCubic;
    etCubicInOut   :   Result := easeInOutCubic;
    etCubicOutIn   :   Result := easeOutInCubic;
    etQuintIn      :   Result := easeInQuint;
    etQuintOut     :   Result := easeOutQuint;
    etQuintInOut   :   Result := easeInOutQuint;
    etQuintOutIn   :   Result := easeOutInQuint;
    etSineIn       :   Result := easeInSine;
    etSineOut      :   Result := easeOutSine;
    etSineInOut    :   Result := easeInOutSine;
    etSineOutIn    :   Result := easeOutInSine;
    etCircIn       :   Result := easeInCirc;
    etCircOut      :   Result := easeOutCirc;
    etCircInOut    :   Result := easeInOutCirc;
    etCircOutIn    :   Result := easeOutInCirc;
    etExpoIn       :   Result := easeInExpo;
    etExpoOut      :   Result := easeOutExpo;
    etExpoInOut    :   Result := easeInOutExpo;
    etExpoOutIn    :   Result := easeOutInExpo;
    etElasticIn    :   Result := easeInElastic;
    etElasticOut   :   Result := easeOutElastic;
    etElasticInOut :   Result := easeInOutElastic;
    etElasticOutIn :   Result := easeOutInElastic;
    etBackIn       :   Result := easeInBack;
    etBackOut      :   Result := easeOutBack;
    etBackInOut    :   Result := easeInOutBack;
    etBackOutIn    :   Result := easeOutBack;
    etBounceIn     :   Result := easeInBounce;
    etBounceOut    :   Result := easeOutBounce;
    etBounceInOut  :   Result := easeInOutBounce;
    etBounceOutIn  :   Result := easeOutInBounce
  else
    Result := nil;
  end;
end;

function Tweener(Current, Target: TAffineVector; Time, Duration: Single; EaseType: TEaseType): TAffineVector;
var
  i: integer;
  EaseFunction : TEaseFunction;
begin
  if Time > Duration then
    Time := Duration;

  EaseFunction := GetEaseFunctionFromType(EaseType);
  for i := 0 to 2 do
  begin
    Target.V[i] := Target.V[i]-Current.V[i];
    Result.V[i] := EaseFunction(Time, Current.V[i], Target.V[i], Duration);
  end;
end;

function Tweener(Current, Target: TGLVector; Time, Duration: Single; EaseType: TEaseType): TGLVector;
var
  i: integer;
  EaseFunction : TEaseFunction;
begin
  if Time > Duration then
    Time := Duration;

  EaseFunction := GetEaseFunctionFromType(EaseType);
  for i := 0 to 3 do
  begin
    Target.V[i] := Target.V[i]-Current.V[i];
    Result.V[i] := EaseFunction(Time, Current.V[i], Target.V[i], Duration);
  end;
end;

function Tweener(Current, Target: TVector2f; Time, Duration: Single; EaseType: TEaseType): TVector2f;
var
  i: integer;
  EaseFunction : TEaseFunction;
begin
  if Time > Duration then
    Time := Duration;

  EaseFunction := GetEaseFunctionFromType(EaseType);
  for i := 0 to 1 do
  begin
    Target.V[i] := Target.V[i]-Current.V[i];
    Result.V[i] := EaseFunction(Time, Current.V[i], Target.V[i], Duration);
  end;
end;

function Tweener(Current, Target: Single; Time, Duration: Single; EaseType: TEaseType): Single;
var
  EaseFunction : TEaseFunction;
begin
  if Time > Duration then
    Time := Duration;

  EaseFunction := GetEaseFunctionFromType(EaseType);

  Result := EaseFunction(Time, Current, Target-Current, Duration);
end;


end.
