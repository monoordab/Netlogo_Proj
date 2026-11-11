;; =====================================================================
;;  نمذجة ومحاكاة توازن جسم صلب - عارضة الأرجوحة / الميزان   (v2)
;;  Rigid-body equilibrium: a beam on a pivot with up to three loads
;;  Based on: Syrian Grade 9 Physics & Chemistry, Unit 2, Lesson 3 (p. 45-50)
;; =====================================================================

globals [
  g dt steps-per-tick scale pivot-y
  theta omega alpha          ; beam angle (rad, + = counter-clockwise), angular velocity, acceleration
  torque-beam torque1 torque2 torque3 net-torque
  inertia reaction sim-time
  contact?                   ; true while the beam rests on the ground
  dragged                    ; the load being dragged with the mouse (or nobody)
  challenge-on? challenge-start challenge-msg challenges-won
  snap-d2                    ; exact balance point after a won challenge (sliders move in 0.05 m steps)
]

breed [ loads a-load ]
breed [ arrows an-arrow ]
breed [ pens a-pen ]
loads-own [ m x tq ]          ; mass (kg), position from the pivot (m, negative = left), torque (N.m)
arrows-own [ owner ]          ; the load (or nobody for the reaction arrow)

;; =====================================================================
;; SETUP
;; =====================================================================
to setup
  let keep-challenge? (is-boolean? challenge-on?) and challenge-on?
  let won ifelse-value (is-number? challenges-won) [ challenges-won ] [ 0 ]
  clear-all
  set challenges-won won
  set g 9.8
  set dt 0.005
  set steps-per-tick 10            ; 1 tick = 0.05 s
  set scale 12
  set pivot-y 4
  set theta initial-angle * pi / 180
  set omega 0
  set sim-time 0
  set contact? false
  set dragged nobody
  set snap-d2 0
  set challenge-on? keep-challenge?
  set challenge-msg ifelse-value challenge-on? [ "Challenge: drag the BLUE load until the beam balances!" ] [ "-" ]

  ;; loads: who 0 = left (red), 1 = right (blue), 2 = third (green, optional)
  create-loads 1 [ set color red   ]
  create-loads 1 [ set color blue  ]
  create-loads 1 [ set color green ]
  ask loads [ set shape "box"  set label-color white ]
  apply-sliders

  ;; weight arrows (one per load) and the pivot reaction arrow
  ask loads [ hatch-arrows 1 [ set owner myself  set shape "arrow"  set heading 180  set label "" ] ]
  create-arrows 1 [ set owner nobody  set shape "arrow"  set heading 0  set color yellow  set label-color yellow ]

  create-pens 1 [ hide-turtle ]
  draw-scene
  compute-torque
  update-view
  reset-ticks
end

;; copy slider values into the loads
to apply-sliders
  ask turtle 0 [ set m m1  set x (0 - d1) ]
  ask turtle 1 [ set m m2  set x ifelse-value (snap-d2 != 0 and (abs (d2 - snap-d2)) < 0.026) [ snap-d2 ] [ d2 ] ]
  ask turtle 2 [
    ifelse third-load? [ set m m3  set x x3  show-turtle ] [ set m 0  set x 0  hide-turtle ]
  ]
  ask loads [ set size 1 + m * 0.35  set label "" ]
end

to draw-scene
  let gy pivot-y - scale * (beam-length / 2 + (abs pivot-offset)) * (sin max-angle) - 0.5
  ask patches with [ pycor < gy ] [ set pcolor 33 ]
  ask patches with [ pxcor = 0 and pycor >= gy and pycor < pivot-y ] [ set pcolor gray - 2 ]
  create-turtles 1 [ setxy 0 pivot-y  set shape "circle"  set color yellow  set size 1 ]
end

;; =====================================================================
;; PHYSICS
;; torque of a weight: Gamma = - m g (x cos(theta) + h sin(theta))
;; (+) counter-clockwise, (-) clockwise  - same convention as the textbook
;; =====================================================================
to compute-torque
  let c cos (theta * 180 / pi)
  let s sin (theta * 180 / pi)
  set torque-beam 0 - beam-mass * g * ((0 - pivot-offset) * c + h * s)
  ask loads [ set tq 0 - m * g * (x * c + h * s) ]
  set torque1 [ tq ] of turtle 0
  set torque2 [ tq ] of turtle 1
  set torque3 [ tq ] of turtle 2
  set net-torque torque-beam + torque1 + torque2 + torque3
  set inertia beam-mass * (beam-length ^ 2 / 12 + pivot-offset ^ 2 + h ^ 2)
              + sum [ m * (x ^ 2 + h ^ 2) ] of loads
  set reaction g * (beam-mass + sum [ m ] of loads)       ; translational equilibrium: R = total weight
end

;; one Euler-Cromer step
to physics-step
  compute-torque
  let lim max-angle * pi / 180
  if contact? [
    ;; the beam rests on the ground: it lifts off only if the torque turns it away from the ground
    ifelse (theta > 0 and net-torque < 0) or (theta < 0 and net-torque > 0)
      [ set contact? false ]
      [ set omega 0  set sim-time sim-time + dt  stop ]
  ]
  set alpha (net-torque - damping * omega) / inertia
  set omega omega + alpha * dt
  set theta theta + omega * dt
  set sim-time sim-time + dt
  if (abs theta) >= lim [
    set theta lim * (ifelse-value (theta > 0) [ 1 ] [ -1 ])
    set omega 0
    set contact? true
  ]
end

to go
  handle-mouse
  apply-sliders
  repeat steps-per-tick [ physics-step ]
  compute-torque
  check-challenge
  update-view
  tick
end

;; =====================================================================
;; MOUSE: drag a load along the beam
;; =====================================================================
to handle-mouse
  ifelse mouse-down? [
    if dragged = nobody [
      let cand loads with [ (not hidden?) and ((distancexy mouse-xcor mouse-ycor) < 2.5) ]
      if any? cand [ set dragged min-one-of cand [ distancexy mouse-xcor mouse-ycor ] ]
    ]
    if dragged != nobody [
      let c cos (theta * 180 / pi)
      let s sin (theta * 180 / pi)
      ;; project the mouse point onto the beam to get the position along it
      let newx (mouse-xcor * c + (mouse-ycor - pivot-y) * s) / scale
      let half beam-length / 2
      set newx max list (0 - pivot-offset - half) (min list (0 - pivot-offset + half) newx)
      set newx (round (newx * 20)) / 20
      let w [ who ] of dragged
      if w = 0 [ set d1 max list 0 (0 - newx) ]
      if w = 1 [ set d2 max list 0 newx ]
      if w = 2 [ set x3 newx ]
    ]
  ] [
    set dragged nobody
  ]
end

;; =====================================================================
;; VIEW
;; =====================================================================
to update-view
  let c cos (theta * 180 / pi)
  let s sin (theta * 180 / pi)
  clear-drawing
  let xa (0 - pivot-offset) - beam-length / 2
  let xb (0 - pivot-offset) + beam-length / 2
  ask pens [
    ;; the beam (short segments so the line never wraps around the world edge)
    set color white  set pen-size 4
    pen-up
    setxy (scale * (xa * c + h * s)) (pivot-y + scale * (xa * s - h * c))
    pen-down
    let k 1
    repeat 10 [
      let xk xa + (xb - xa) * k / 10
      setxy (scale * (xk * c + h * s)) (pivot-y + scale * (xk * s - h * c))
      set k k + 1
    ]
    pen-up
    ;; ruler marks every 0.1 m, longer every 0.5 m
    set color gray  set pen-size 1
    let xm (ceiling (xa * 10)) / 10
    while [ xm <= xb + 0.0001 ] [
      let len ifelse-value ((abs (round (xm * 10)) mod 5) = 0) [ 0.9 ] [ 0.45 ]
      let px scale * (xm * c + h * s)
      let py pivot-y + scale * (xm * s - h * c)
      setxy (px + len * s) (py - len * c)
      pen-down
      setxy (px - len * s) (py + len * c)
      pen-up
      set xm xm + 0.1
    ]
  ]
  ;; loads sit on top of the beam
  ask loads [
    let px scale * (x * c + h * s)
    let py pivot-y + scale * (x * s - h * c)
    setxy (px - (size / 2) * s) (py + (size / 2) * c)
    set heading theta * -180 / pi
  ]
  ;; weight arrows hang under each load, reaction arrow points up from the pivot
  ask arrows [
    ifelse owner = nobody [
      set size min list 6 (reaction / 15)
      setxy 0 (pivot-y + size / 2 + 0.6)
      set label (word "R = " precision reaction 1 " N")
    ] [
      ifelse ([ hidden? ] of owner) or (([ m ] of owner) = 0) [ hide-turtle ] [
        show-turtle
        set color [ color ] of owner
        set size 0.8 + [ m ] of owner * 0.7
        let ox [ xcor ] of owner
        let oy [ ycor ] of owner - [ size ] of owner / 2 - size / 2
        ifelse oy - size / 2 > min-pycor [ setxy ox oy ] [ setxy ox (min-pycor + size / 2) ]
        let t [ tq ] of owner
        set label (word ([ m ] of owner) " kg  T=" (ifelse-value (t > 0) [ "+" ] [ "" ]) precision t 1)
      ]
    ]
  ]
end

;; =====================================================================
;; BUTTONS
;; =====================================================================

;; find d2 that makes the sum of torques zero (beam horizontal)
to auto-balance
  let m3-now ifelse-value third-load? [ m3 ] [ 0 ]
  let need (m1 * d1 + beam-mass * pivot-offset - m3-now * x3) / m2
  let max-d2 beam-length / 2 - pivot-offset
  ifelse need >= 0 and need <= max-d2 [
    set d2 precision need 3
    set initial-angle 0
    setup
    set challenge-msg (word "auto-balance: d2 = m1 x d1 / m2 = " precision need 3 " m  -> sum of torques = 0")
  ] [
    ;; no position on the beam can balance it: explain why in the Messages box
    ifelse need > max-d2 [
      set challenge-msg (word "Not possible: needs d2 = " precision need 2
        " m but the beam ends at " precision max-d2 2 " m  ->  set d1 <= "
        precision (max list 0 ((m2 * max-d2 - beam-mass * pivot-offset + m3-now * x3) / max list 0.001 m1)) 2
        " m  or  m2 >= " precision ((m1 * d1 + beam-mass * pivot-offset - m3-now * x3) / max-d2) 2 " kg")
    ] [
      set challenge-msg "Not possible: the other loads already turn the beam to the right. Move or lighten them."
    ]
  ]
end

;; ready-made experiments
to load-scenario
  set third-load? false
  set beam-mass 2  set beam-length 2  set damping 0
  if scenario = "1. Balanced seesaw"       [ set-params 2 0.5 1 1.0 0 0 0 ]
  if scenario = "2. Unbalanced - tips left" [ set-params 2 0.5 1 0.8 0 0 0 ]
  if scenario = "3. Heavy vs light"         [ set-params 3 0.8 1 0.5 0 0 0 ]
  if scenario = "4. Stable equilibrium"     [ set-params 2 0.5 1 1.0 0.1 5 0 ]
  if scenario = "5. Neutral equilibrium"    [ set-params 2 0.5 1 1.0 0 5 0 ]
  if scenario = "6. Unstable equilibrium"   [ set-params 2 0.5 1 1.0 -0.1 5 0 ]
  if scenario = "7. Off-centre pivot"       [ set-params 0 0.5 0.5 0.8 0 0 0.2 ]
  if scenario = "8. Three loads"            [
    set-params 2 0.3 1.5 1.0 0 0 0
    set third-load? true  set m3 1  set x3 -0.9
  ]
  set challenge-on? false
  setup
end

to set-params [ a-m1 a-d1 a-m2 a-d2 a-h a-angle a-offset ]
  set m1 a-m1  set d1 a-d1  set m2 a-m2  set d2 a-d2
  set h a-h  set initial-angle a-angle  set pivot-offset a-offset
end

;; game: random left load, the player must balance the beam with the blue load
to new-challenge
  set third-load? false
  set h 0  set pivot-offset 0  set initial-angle 0  set damping 0
  set beam-mass 2  set beam-length 2
  let ok? false
  while [ not ok? ] [
    set m1 precision (0.5 + random-float 3) 1
    set d1 (2 + random 15) / 20
    set m2 precision (0.5 + random-float 3) 1
    let need m1 * d1 / m2
    if need >= 0.1 and need <= 0.95 [ set ok? true ]
  ]
  set d2 (1 + random 19) / 20
  set challenge-on? true
  setup
  set challenge-start 0
end

to check-challenge
  if challenge-on? [
    if (abs net-torque) <= m2 * g * 0.026 [
      set challenge-on? false
      set challenges-won challenges-won + 1
      ;; snap the blue load to the exact balance point (the slider moves in 0.05 m steps)
      set snap-d2 m1 * d1 / m2
      apply-sliders
      set challenge-msg (word "BALANCED! m1 x d1 = " precision (m1 * d1) 3 " = m2 x d2 = " precision (m2 * snap-d2) 3
                         "   (" precision sim-time 1 " s)   Wins: " challenges-won)
      ;; bring the beam back to horizontal for a clear view
      set theta 0  set omega 0  set contact? false
    ]
  ]
end

;; =====================================================================
;; REPORTERS
;; =====================================================================
to-report angle-deg
  report theta * 180 / pi
end

to-report status
  if contact? and (abs net-torque) < 0.01 [
    report "BALANCED (tilted) - setup to level"
  ]
  if contact? [
    report ifelse-value (theta > 0) [ "On the ground - LEFT side down" ] [ "On the ground - RIGHT side down" ]
  ]
  if (abs net-torque) < 0.0001 and (abs omega) < 0.000001 [ report "BALANCED: sum of torques = 0" ]
  report ifelse-value (net-torque > 0) [ "Turning left (net torque > 0)" ] [ "Turning right (net torque < 0)" ]
end
@#$#@#$#@
GRAPHICS-WINDOW
215
10
652
448
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
10
10
105
45
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
110
10
205
45
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

BUTTON
10
50
205
85
auto-balance (find d2)
auto-balance
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
95
205
128
m1
m1
0
5
2.0
0.1
1
kg
HORIZONTAL

SLIDER
10
130
205
163
d1
d1
0
1.2
0.5
0.05
1
m
HORIZONTAL

SLIDER
10
170
205
203
m2
m2
0.1
5
1.0
0.1
1
kg
HORIZONTAL

SLIDER
10
205
205
238
d2
d2
0
1.2
0.8
0.05
1
m
HORIZONTAL

SLIDER
10
245
205
278
beam-mass
beam-mass
0
5
2.0
0.1
1
kg
HORIZONTAL

SLIDER
10
280
205
313
beam-length
beam-length
1
2.4
2.0
0.1
1
m
HORIZONTAL

SLIDER
10
320
205
353
h
h
-0.3
0.3
0.0
0.05
1
m
HORIZONTAL

SLIDER
10
355
205
388
pivot-offset
pivot-offset
-0.4
0.4
0.0
0.05
1
m
HORIZONTAL

SLIDER
10
395
205
428
initial-angle
initial-angle
-15
15
0.0
1
1
deg
HORIZONTAL

SLIDER
10
430
205
463
damping
damping
0
2
0.0
0.05
1
N.m.s
HORIZONTAL

SLIDER
10
465
205
498
max-angle
max-angle
5
40
20.0
1
1
deg
HORIZONTAL

MONITOR
215
455
430
500
Status
status
0
1
11

MONITOR
435
455
545
500
Net torque (N.m)
net-torque
3
1
11

MONITOR
550
455
652
500
Angle (deg)
angle-deg
2
1
11

MONITOR
215
505
320
550
Torque left (N.m)
torque1
3
1
11

MONITOR
325
505
430
550
Torque right (N.m)
torque2
3
1
11

MONITOR
435
505
545
550
Pivot reaction R (N)
reaction
2
1
11

MONITOR
550
505
652
550
Time (s)
sim-time
2
1
11

MONITOR
215
565
1180
610
Messages
challenge-msg
0
1
11

PLOT
660
10
960
220
Beam angle
time (s)
angle (deg)
0.0
5.0
-20.0
20.0
true
false
"" ""
PENS
"angle" 1.0 0 -13345367 true "" "plotxy sim-time angle-deg"

PLOT
660
230
960
440
Torques
time (s)
torque (N.m)
0.0
5.0
-10.0
10.0
true
true
"" ""
PENS
"left" 1.0 0 -2674135 true "" "plotxy sim-time torque1"
"right" 1.0 0 -13345367 true "" "plotxy sim-time torque2"
"net" 1.0 0 -16777216 true "" "plotxy sim-time net-torque"

CHOOSER
970
10
1180
55
scenario
scenario
"1. Balanced seesaw" "2. Unbalanced - tips left" "3. Heavy vs light" "4. Stable equilibrium" "5. Neutral equilibrium" "6. Unstable equilibrium" "7. Off-centre pivot" "8. Three loads"
0

BUTTON
970
60
1180
95
load scenario
load-scenario
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
970
110
1180
143
third-load?
third-load?
1
1
-1000

SLIDER
970
145
1180
178
m3
m3
0
5
1.0
0.1
1
kg
HORIZONTAL

SLIDER
970
180
1180
213
x3
x3
-1.2
1.2
0.4
0.05
1
m
HORIZONTAL

BUTTON
970
230
1180
270
new challenge (game)
new-challenge
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
970
280
1180
440
HOW TO PLAY\n1. Press GO (keep it running).\n2. DRAG any box along the beam with the mouse - or use the sliders.\n3. Arrows show each weight and its torque T; the yellow arrow is the pivot reaction R.\nChallenge: balance the beam using only the BLUE box.
11
0.0
1

TEXTBOX
660
450
960
560
Torque convention (as in the textbook):\n(+) counter-clockwise, (-) clockwise.\nBalance: sum of torques = 0  (m1 x d1 = m2 x d2)\nh > 0: pivot above centre of gravity (stable)\nh = 0: neutral   |   h negative: unstable
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

An interactive model of a rigid beam (seesaw / balance) that can rotate about a pivot, with up to three loads placed on it. It is based on the lesson "Equilibrium of a rigid body" (توازن جسم صلب) - Syrian Grade 9 Physics & Chemistry textbook, Unit 2, pages 45-50.

The two equilibrium conditions from the textbook:

* Translational equilibrium: sum of forces = 0  (the pivot reaction R equals the total weight)
* Rotational equilibrium: sum of torques = 0  (m1 x d1 = m2 x d2)

## HOW IT WORKS

Each load is an agent with a mass `m` and a position `x` measured from the pivot (negative = left).
The torque of each weight is  T = - m g (x cos(theta) + h sin(theta)).
The beam's angular acceleration is  alpha = (sum of torques - damping x omega) / I,
integrated with the Euler-Cromer method (dt = 0.005 s). When the beam touches the ground it rests there until the torques turn it back up.

## HOW TO USE IT

1. Pick a scenario and press LOAD SCENARIO, or set the sliders and press SETUP.
2. Press GO and keep it running.
3. Drag any box along the beam with the mouse and watch the torques, the arrows and the plots change.
4. AUTO-BALANCE computes the right-load position that makes the sum of torques zero.
5. NEW CHALLENGE starts a game: balance the beam using only the blue box.

## THINGS TO TRY

* Scenario 1: balanced, 2 kg x 0.5 m = 1 kg x 1.0 m.
* Scenarios 4-6: stable, neutral and unstable equilibrium (pivot above, at, or below the centre of gravity).
* Scenario 7: the beam's own weight produces a torque when the pivot is not at its centre.
* Scenario 8: three loads - the sum of all torques must be zero.

## CREDITS

University Modeling & Simulation course project (2026-2027).
NetLogo: Wilensky, U. (1999). http://ccl.northwestern.edu/netlogo/
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

circle
false
0
Circle -7500403 true true 0 0 300
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
