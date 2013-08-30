
Perfect universe
----------------
All systems running in parallel, eating the smallest units of time 
possible (timelets!) and excreting the result.

IS time consumed/used...or like water in a mill

its likea water mill, the water doesnt vanish, it comes out the other side and into (in our case) the next water wheel.

Real World
----------
Now our perfect universe its perfectly paralell but out game is not
when the render, physics, etc wheels are turning (processing is 
happening), the water (time) cannot keep flowing through, so it collects
in the damn at the top.

Our metaphor breaks into tiny fecking pieces here.

Each wheel has a minimum ammount of water to make it turn (timestep)
and so it stores up time until it has enough...however the time still 
goes to the next wheel.

when the wheels are done turning, we return to the start of the loop 
where a check of the high-res timer tells us how much time has been
dammed up (cached) while we were busy. This is fed into the wheels and
we are off again!

----------------------------------------------------------

Spiral of Death from "Fix Your Timestep!"

What exactly is this spiral of death? It’s what happens when 
your physics simulation cannot keep up with the steps it’s 
asked to take.

For example, if your simulation is told: “OK, please simulate X seconds
worth of physics” and if it takes Y seconds of real time to do so where
Y > X, then it doesn’t take Einstein to realize that over time your
simulation falls behind. It’s called the spiral of death because 
ironically being behind causes your update to simulate more steps, 
which causes you to fall further behind, which makes you simulate more
steps...

So how do we avoid this? In order to ensure a stable update I recommend
leaving some headroom. You really need to ensure that it takes 
*significantly less* than X seconds of real time to update X seconds 
worth of physics simulation. If you can do this then your physics 
engine can “catch up” from any temporary spike by simulating more 
frames. Alternatively you can clamp at a maximum # of steps per-frame 
and the simulation will appear to slow down under heavy load. Arguably 
this is better than spiraling to death, assuming of course that the 
heavy load is just a temporary spike.

==========

What can we draw from this? Well some kind of balancing error would be 
nice I guess....do we auto adapt if requested?
We could also have a limit field so we can do the step clamp metioned 
above.

----------------------------------------------------------

Temporal Aliasing from "Fix Your Timestep!"

Now consider that in general all render frames will have some small 
remainder of frame time left in the accumulator that cannot be simulated
because it is less than dt. What this means is that we’re displaying the
state of the physics simulation at a time value slightly different from
the render time. This causes a subtle but visually unpleasant stuttering
of the physics simulation on the screen known as temporal aliasing.

The solution is to interpolate between the previous and current physics
state based on how much time is left in the accumulator.

==========

OK, so we need this in our system.
This is tricky as we need some kind of state caching to interpolate 
between...hmmm, I'll come back to this.

----------------------------------------------------------

(game-loop
 (handle-events
  :forward-key (move-character-forward))
 (with-time
     ((render t!time) :timestep 0.016666)
     ((physics) :timestep 0.005 :max-steps 200)))

;; not sure what I'm thinking about with the 'T!' symbols...but it
;; seems interesting. Ho hum.

;; what is the best format to tell handle-events that any events of a 
;; certain class (e.g. joystick) should be given to function 'x'
;; (joystick-handler for example)
