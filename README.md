# SqLego
A little package for controling Lego MindStorms RCX system directly from Squeak, inspired by the Bot-Kit interface.

Original website – http://wiki.squeak.org/squeak/2412

There are two different ways for controlling an RCX
* Immediate Mode: send order and see result
* Program Mode: download a program to the lego, press run and watch

Before testing anything, open a transcript, this will show debug information.

## Modes

### Immediate Mode
The small examples should be explain enought I guess :

```
rcx := RCX onPort: 0. "the IRtower is plugged on serial port 0 (unix) and 1 (windows)"
rcx soundBeep. "If you hear something, everything is okay !"
rcx closePort.
```

Try to use the motors :
```
rcx := RCX onPort: 0. "the IRtower is plugged on serial port 0"
motor := rcx motor: 1. "We get an instance for the motor 1"
motor start. "If the first motor start to turn, everything is ok"
motor float.
rcx closePort.
```

Of course, you can use several motors at the same time :
```
rcx := RCX onPort: 0. "the IRtower is plugged on serial port 0"
motor1 := rcx motor: 1. "We get an instance for the motor 1"
motor2 := rcx motor: 3. "We get an instance for the motor 3"
motor2 start.
motor1 start.

motor2 stop.
motor1 float.
rcx closePort.
```

That's all for the immediate mode.

###Program mode

For compiling and downloading a little program to the RCX use the method :
```
RCX downloadProgram: aProgram as: aNumber onPort: aPortNumber
```

aProgram is a collection of instruction, aNumber is a number between 1 and 5 corresponding and the port reference the serial port (typically 0 for the first one).

The program is a collection of instructions which could be :
```
soundBeep
soundClick
soundFastSweepUp
soundSweepDown
soundSweepUp
soundError

turnOnLeft
turnOnMiddle
turnOnRight

turnOffLeft
turnOffMiddle
turnOffRight

wait aNumber
powerMotor motorNumber powerNumber
```

where aNumber is a real positive number designing the number of second to wait.
motorNumber is a number between 1 and 3, and powerNumber between 0 and 7

For example, doit this:
```
r:=#( #(soundBeep) #(wait 3.5) #(soundClick) #(turnOnLeft) #(wait 2) #(turnOffLeft)).
RCX downloadProgram: r as: 1 onPort: 0 "onPort: 1 – for under windows"
```
And then press the green button "run".


## Files
The changeset:
* RCX.cs
  - File in this file to enjoy with Lego MindStorms

Some fixes from Ned Konz to load after the above
* RCXFixes-nk.cs

### An eToy Scriptable RCX brick Morph
1. Load the RCX changeset,
2. then the RCXFixes-nk changeset,
3. then download the RCXMorph-nk changeset and the GIF file into your Squeak directory,
4. then load the RCXMorph-nk changeset.


## Links
Bot-Kit – http://www.object-arts.com/Bower/Bot-Kit/


## Contacts
Bergel Alexandre : bergel (at) iam.unibe.ch – last update 05 May 2002
Ned Konz : ned (at) bike-nomad.com – last update 19 Aug 2002
