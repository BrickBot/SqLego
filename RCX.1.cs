'From Squeak3.2gamma of 15 January 2002 [latest update: #4811] on 31 July 2002 at 1:51:39 pm'!
Object subclass: #RCX
	instanceVariableNames: 'port irEncoder motorsList '
	classVariableNames: 'RCXsActive '
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
RCX class
	instanceVariableNames: ''!
PolygonMorph subclass: #RCXCar
	instanceVariableNames: 'path plotsToReach moving headingToGet program '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCXIde'!
RCXCar class
	instanceVariableNames: ''!
Object subclass: #RCXCompiler
	instanceVariableNames: 'source target vitesse rayon coeffTurn coeffWalk '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCXCompiler'!
RCXCompiler class
	instanceVariableNames: ''!
Object subclass: #RCXIrEncoder
	instanceVariableNames: 'lastCommand lastCommandArray '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!

!RCXIrEncoder commentStamp: '<historical>' prior: 0!
If you send two times the same command, you have to set on a bit on the latter send. So, lastCommandArray is used for storing the command sent.!
]style[(143)f1cmagenta;!

Object subclass: #RCXLight
	instanceVariableNames: 'rcx light '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
Object subclass: #RCXMotor
	instanceVariableNames: 'plug rcx direction started '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
RCXMotor class
	instanceVariableNames: ''!
SimpleButtonMorph subclass: #RCXPathGenerator
	instanceVariableNames: 'value '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCXIde'!
EllipseMorph subclass: #RCXPlot
	instanceVariableNames: 'value '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCXIde'!
RCXPlot class
	instanceVariableNames: ''!
Object subclass: #RCXSerialPortReader
	instanceVariableNames: 'sharedQueue serialPort process logging portNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
RCXSerialPortReader class
	instanceVariableNames: ''!
TestCase subclass: #RCXTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
Object subclass: #RCXTouchSensor
	instanceVariableNames: 'rcx plug '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
RCXTouchSensor class
	instanceVariableNames: ''!
PasteUpMorph subclass: #RCXWorld
	instanceVariableNames: 'value plots car '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCXIde'!
RCXWorld class
	instanceVariableNames: ''!
SimpleButtonMorph subclass: #RCXWorldButton
	instanceVariableNames: 'action '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCXIde'!
RCXWorldButton class
	instanceVariableNames: ''!

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/16/2002 19:11'!
byteCodeForClearTimer: aNumber 
	"aNumber is between 0 and 3, design the timer number"
	^ ByteArray with: 161 with: aNumber!
]style[(23 7 3 53 4 9 7 3 7 7)f1b,f1cblack;b,f1,f1c124022000,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/16/2002 19:15'!
byteCodeForPlaySystemSound: aNumber 
	"aNumber is between 0 and 5"
	^ ByteArray with: 16r51 with: aNumber!
]style[(35 3 28 4 9 19 7)f1b,f1,f1c122020000,f1,f1cblack;,f1,f1cblack;! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/16/2002 18:39'!
byteCodeForSelectProgram: aNumber

	^ByteArray with: 16r91 with: (aNumber)! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/20/2002 19:12'!
byteCodeForSetPowerMotor: motorNumber source: sourceNumber power: powerNumber 
	"motorNumber between  
	sourceNumber 0, 2 or 4 
	powerNumber argument for the source"
	^ ByteArray
		with: 19
		with: motorNumber
		with: sourceNumber
		with: powerNumber!
]style[(26 11 9 12 8 11 3 85 4 9 9 2 9 11 9 12 9 11)f1b,f1cblack;b,f1b,f1cblack;b,f1b,f1cblack;b,f1,f1c124022000,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/16/2002 18:37'!
byteCodeForStartTaskDownloadingWithTask: taskNumber andLength: taskLength 
	"taskNumber between 0 and 9"
	| taskNumberHigh taskNumberLow taskLengthHigh taskLengthLow |
	taskNumberHigh _ (taskNumber bitAnd: 65280)
				>> 8.
	taskNumberLow _ taskNumber bitAnd: 255.
	taskLengthHigh _ (taskLength bitAnd: 65280)
				>> 8.
	taskLengthLow _ taskLength bitAnd: 255.
	^ ByteArray
				with: 37
				with: 0
				with: taskNumberLow
				with: taskNumberHigh
				with: taskLengthLow
				with: taskLengthHigh.!
]style[(41 10 12 10 3 28 3 58 4 14 4 10 9 5 9 1 3 13 3 10 9 3 3 14 4 10 9 5 9 1 3 13 3 10 9 3 5 9 11 2 11 1 11 13 11 14 11 13 11 14 1)f1b,f1cblack;b,f1b,f1cblack;b,f1,f1c123021000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblue;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/16/2002 18:35'!
byteCodeForTransfertForIndex: index data: aProgram 
	"aProgram is an ByteArray"
	| length lengthHigh lengthLow indexHigh indexLow |
	length _ aProgram size.
	lengthHigh _ (length bitAnd: 65280)
				>> 8.
	lengthLow _ length bitAnd: 255.
	indexHigh _ (index bitAnd: 65280)
				>> 8.
	indexLow _ index bitAnd: 255.
	^ (ByteArray
		with: 69
		with: indexLow
		with: indexHigh
		with: lengthLow
		with: lengthHigh)
		, aProgram
		, (ByteArray
				with: (self class getChecksum: aProgram))!
]style[(30 5 7 8 3 26 3 47 4 6 3 8 8 10 4 6 9 5 9 1 3 9 3 6 9 3 3 9 4 5 9 5 9 1 3 8 3 5 9 3 6 9 9 2 9 8 9 9 9 9 9 10 6 8 6 9 12 4 20 8 2)f1b,f1cblack;b,f1b,f1cblack;b,f1,f1c124022000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/20/2002 18:03'!
byteCodeForTurnOffMotor: aNumber 
	| v |
	"aNumber is between 1 and 3"
	v_RCXMotor getByteCodeFromMotor: aNumber motorStatus: 1.
	^ (ByteArray with: 16r21 with: v)! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/19/2002 13:40'!
byteCodeForTurnOnMotor: aNumber 
	| v |
	"aNumber is between 0 and 3, design the timer number"
	v_RCXMotor getByteCodeFromMotor: aNumber motorStatus: 2.
	^ (ByteArray with: 16r21 with: v)! !

!RCX methodsFor: 'ByteCode generator' stamp: 'AB 1/19/2002 14:00'!
byteCodeForWait: sourceNumber arg: argNumber 
	"sourceNumber is a byte, and argNumber a word"
	"sourceNumber : 
		0 variable, arg variable number (0 - 31) 
		2 imediate value, arg value 
		4 random, arg maximal bound"
	"The delay is in 1/100ths of a second"
	| argHigh argLow |
	argHigh _ (argNumber bitAnd: 65280)
				>> 8.
	argLow _ argNumber bitAnd: 255.
	^ ByteArray
		with: 67
		with: sourceNumber
		with: argLow
		with: argHigh!
]style[(17 12 6 9 3 210 3 15 4 7 4 9 9 5 9 1 3 6 3 9 9 3 5 9 9 11 12 9 6 9 7)f1b,f1cblack;b,f1b,f1cblack;b,f1,f1c123021000,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblue;,f1cblack;b,f1,f1cblack;,f1,f1cblack;! !

!RCX methodsFor: 'accessing' stamp: 'nk 4/6/2002 21:25'!
portNumber
	^ port portNumber! !

!RCX methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:12'!
initialize

	motorsList _ OrderedCollection new.! !

!RCX methodsFor: 'private' stamp: 'nk 4/6/2002 16:11'!
closePort
	port close.
	self class activeRCXs remove: self!
]style[(9 2 4 43)f1b,f1,f1cblack;,f1! !

!RCX methodsFor: 'private' stamp: 'nk 4/6/2002 12:40'!
doInitAlive
	| r |
	r_#(85 255 0 16 239 16 239) asByteArray.
	port nextPutAll: r.
	irEncoder setLastCommandArray: r.
	r _ self read.
	^ (r at: 1)
		= 231! !

!RCX methodsFor: 'private' stamp: 'AB 5/5/2002 15:07'!
initializeOnPort: portNumber 
	motorsList _ OrderedCollection new.
	port _ RCXSerialPortReader onPort: portNumber.
	irEncoder _ RCXIrEncoder new.
	"self doInitAlive"
	self isAlive! !

!RCX methodsFor: 'private' stamp: 'nk 4/6/2002 13:07'!
log: anObject
	Transcript nextPutAll: anObject asString; cr! !

!RCX methodsFor: 'private' stamp: 'nk 4/6/2002 16:09'!
reopenPort
	port reopen! !

!RCX methodsFor: 'private' stamp: 'nk 4/6/2002 16:11'!
shutDownPort
	port close.! !

!RCX methodsFor: 'operation' stamp: 'nk 4/6/2002 15:07'!
enableLogging: aBoolean
	port enableLogging: aBoolean! !

!RCX methodsFor: 'operation' stamp: 'AB 5/5/2002 21:49'!
isAlive
	| r |
	r _ self sendAndRead: #(16).
	^ (r at: 1)
		= 231! !

!RCX methodsFor: 'operation' stamp: 'nk 4/6/2002 20:05'!
motor: aNumber

	| motor |
	motor _ motorsList
		detect: [ :ea | ea plug == aNumber ]
		ifNone: [ motorsList add: (RCXMotor onPlug: aNumber rcx: self) ].
	^motor.! !

!RCX methodsFor: 'operation' stamp: 'AB 5/5/2002 21:50'!
sound: aNumber 
	^(self
		sendAndRead: (self byteCodeForPlaySystemSound: aNumber)) first = 166
! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:52'!
soundBeep
	self sound: 1.! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:52'!
soundClick
	self sound: 0.! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:54'!
soundError
	self sound: 4.! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:53'!
soundFastSweepUp
	self sound: 5.! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:55'!
soundSweepDown
	self sound: 2.! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:55'!
soundSweepUp
	self sound: 3.! !

!RCX methodsFor: 'operation' stamp: 'AB 1/15/2002 16:23'!
touchSensor: aNumber

	^(RCXTouchSensor onPlug: aNumber rcx: self).! !

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 14:53'!
downloadRawProgram: byteCodes as: aNumber
	self log: (self selectProgram: aNumber).
	self log: (self startTaskDownloadingWithTask: 0 andLength: byteCodes size) asArray.
	self log: (self transfertForIndex: 0 data: byteCodes) asArray.
! !

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 13:45'!
flush
	port flush! !

!RCX methodsFor: 'transferring' stamp: 'AB 5/5/2002 16:47'!
read
	(Delay forMilliseconds: 300) wait.
	^ irEncoder decode: port readByteArray! !

!RCX methodsFor: 'transferring' stamp: 'AB 5/5/2002 21:50'!
selectProgram: aNumber 
	"Arg between 1 and 5"
	^(self
		sendAndRead: (self byteCodeForSelectProgram: aNumber - 1)) first = 102.! !

!RCX methodsFor: 'transferring' stamp: 'AB 5/5/2002 16:47'!
send: aByteArray 
	| r |
	r _ irEncoder encode: aByteArray.
		(Delay forMilliseconds: 300) wait.
	port nextPutAll: r! !

!RCX methodsFor: 'transferring' stamp: 'AB 5/5/2002 21:48'!
sendAndRead: aByteArray 
	"Use an exception mechanism"
	"Perhaps a problem for rcx memory dumping. Perhaps have to wait a bit"
	|try|
	try _ 0.
	[self send: aByteArray.
	^ self read]
	on: Exception
	do: [:ex| Transcript show: 'Exception thrown : ', ex messageText ;cr. try _ try + 1.
		(try > 3 ) ifTrue: [ Exception signal: 'Communication aborded !!'] ifFalse: [ ex retry. ]]! !

!RCX methodsFor: 'transferring' stamp: 'AB 5/5/2002 21:51'!
startTaskDownloadingWithTask: taskNumber andLength: taskLength 
	^(self
		sendAndRead: (self byteCodeForStartTaskDownloadingWithTask: taskNumber andLength: taskLength))
! !

!RCX methodsFor: 'transferring' stamp: 'AB 5/5/2002 21:51'!
transfertForIndex: index data: aProgram 
	^self
		sendAndRead: (self byteCodeForTransfertForIndex: index data: aProgram)! !

!RCX methodsFor: 'as yet unclassified' stamp: 'AB 3/28/2002 21:54'!
onPort: portNumber 
	port _ SerialPort new.
	port baudRate: 2400.
	port dataBits: 8.
	port parityType: 1.
	port stopBitsType: 1.
	port openPort: portNumber.
	"port _ RCXSerialPortReader createFromSerialPort: port."
	irEncoder _ RCXIrEncoder new.
	self doInitAlive! !


!RCX class methodsFor: 'Examples' stamp: 'AB 4/30/2002 15:18'!
downloadProgram: aProgram as: aNumber 
	"self downloadProgram: #(#(#soundClick) ) as: 1."
	self
		downloadProgram: aProgram
		as: aNumber
		onPort: 1! !

!RCX class methodsFor: 'Examples' stamp: 'AB 3/28/2002 16:28'!
downloadProgram: aProgram as: aNumber onPort: aPortNumber 
	| code |
	code _ RCXCompiler compile: aProgram.
	self downloadRawProgram: code as: aNumber onPort: aPortNumber.
! !

!RCX class methodsFor: 'Examples' stamp: 'nk 4/6/2002 21:33'!
downloadRawProgram: byteCodes as: aNumber onPort: aPortNumber 
	| rcx |
	rcx _ self onPort: aPortNumber.
	rcx isAlive.
	self log: (rcx selectProgram: aNumber).
	self log: (rcx startTaskDownloadingWithTask: 0 andLength: byteCodes size) asArray.
	self log: (rcx transfertForIndex: 0 data: byteCodes) asArray.
! !

!RCX class methodsFor: 'Examples' stamp: 'nk 4/6/2002 21:34'!
example1
	"RCX example1"
	| rcx p |
	rcx _ self onPort: 0.
	rcx isAlive.
	rcx soundSweepUp.
	p _ ((rcx byteCodeForTurnOnMotor: 1)
				, (rcx byteCodeForTurnOnMotor: 3)
				, (rcx byteCodeForWait: 2 arg: 300)
				, (rcx byteCodeForTurnOffMotor: 1)
				, (rcx byteCodeForTurnOffMotor: 3)) asByteArray.
	self log: (rcx selectProgram: 1).
	self log: (rcx startTaskDownloadingWithTask: 0 andLength: p size) asArray.
	self log: (rcx transfertForIndex: 0 data: p) asArray.
! !

!RCX class methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 15:58'!
activeRCXs

	^RCXsActive ifNil: [ RCXsActive _ Set new ].! !

!RCX class methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:11'!
shutDown

	self activeRCXs do: [ :rcx | rcx shutDownPort ]! !

!RCX class methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:09'!
startUp

	self activeRCXs do: [ :rcx | rcx reopenPort ]! !

!RCX class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:29'!
onPort: portNumber
	| newMe |
	newMe _ self activeRCXs
		detect: [ :ea | ea portNumber == portNumber ]
		ifNone: [
			newMe _ super new initializeOnPort: portNumber.
			self activeRCXs add: newMe.
		].
	^newMe! !

!RCX class methodsFor: 'utilities' stamp: 'AB 1/15/2002 18:31'!
getChecksum: aByteArray

	| r |
	r_0.
	aByteArray do: [:v| r_r+v].
	^r bitAnd: 16rFF.! !

!RCX class methodsFor: 'utilities' stamp: 'AB 3/15/2002 14:38'!
log: anObject 
	Transcript show: anObject asString! !

!RCX class methodsFor: 'as yet unclassified' stamp: 'AB 1/15/2002 16:37'!
new

	^super new initialize! !


!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/20/2002 17:56'!
addFirst: instr

	program addFirst: instr!
]style[(15 3 7 16)f1b,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/18/2002 22:22'!
addInstruction: instr 
	program isNil
		ifTrue: [program _ OrderedCollection new.
			].
program add: instr!
]style[(16 5 3 7 18 7 3 17 12 7 6 5)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cblue;i! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 2/17/2002 13:04'!
compileProgram: aPath 
	| walkerPos walkerHeading |
	walkerPos _ aPath first center.
	walkerHeading _ (aPath at: 2) center - (aPath at: 1) center.
	walkerHeading _ (walkerHeading x @ walkerHeading y negated) degrees.
	aPath allButFirst
		do: [:p | 
			self addInstruction: (self getTurnInstructionBy: ((self getAngleOn: p center and: walkerPos) - walkerHeading \\ 360)).

			walkerHeading _ self getAngleOn: p center and: walkerPos.
			self
				addInstruction: (Array with: #walk with: (walkerPos - p center) r).
			walkerPos _ p center].
	self removeFirstInstr! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/18/2002 22:45'!
getAngleOn: p1 and: p2 
	| t |
	t _ p1 - p2.
	t _ t x @ t y negated.
	^ t degrees!
]style[(12 2 6 2 4 2 4 1 3 2 3 3 2 1 3 1 5 1 15 1 8)f1b,f1cblue;b,f1b,f1cblue;b,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/19/2002 11:54'!
getCompiledProgram

	^program! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/18/2002 21:48'!
getProgram
	^program! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 2/17/2002 13:05'!
getTurnInstructionBy: angle

	^(angle > 180)
		ifTrue: [Array with: #turnRight with: (360 - angle)]
		ifFalse: [Array with: #turnLeft with: angle].! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/18/2002 21:52'!
lastCommand

	^program last! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/19/2002 13:16'!
removeFirstInstr

	program removeFirst!
]style[(19 7 12)f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'compilation process' stamp: 'AB 1/18/2002 21:48'!
resetProgram

	program_OrderedCollection new.! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 18:23'!
directionOfPoint: aPoint 
	| vec |
	vec _ aPoint - self center.
	vec _ vec x @ vec y negated.
	self heading: 90 - vec degrees!
]style[(18 6 4 4 4 3 3 6 3 4 10 3 3 3 5 3 13 4 15 3 8)f1b,f1cblue;b,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cblue;i,f1! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 21:55'!
doNothing
! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 20:08'!
drawOn: aCanvas 
	super drawOn: aCanvas.
"	aCanvas
		fillOval: (self realPosition - (5 @ 5) extent: self realPosition + (5 @ 5))
		color: Color white"!
]style[(8 7 3 5 9 7 4 7 14 4 17 1 3 1 10 4 17 1 3 1 12 5 7)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1,f1c172172096,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096,f1,f1c172172096,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 18:53'!
initPosition: aPoint 

	self rotationCenter: 0.5 @ 0.
	self position: aPoint- (self width / 2 @ 0)!
]style[(14 6 4 4 17 3 3 1 18 6 22)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1c170170094,f1,f1c170170094,f1,f1cblue;i,f1c120018000! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 20:20'!
initialize
	super initialize.
	self width: 10.
	self height: 30.
	self
		vertices: (Array
				with: 0 @ self height
				with: self width / 2 @ 0
				with: self width @ self height)
		color: Color blue
		borderWidth: 2
		borderColor: Color red.
	moving _ 0!
]style[(10 2 5 14 4 8 2 3 4 9 2 3 4 14 5 11 1 3 4 18 4 9 1 3 1 11 4 9 4 18 5 21 1 16 5 7 6 4)f1b,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 18:30'!
path: aPath 
	path _ aPath!
]style[(6 5 3 4 3 5)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cblue;i! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 20:05'!
realPosition
	| r |
	r _ self rotationCenter.
	^ self width * r x @ (self height * r y) + self position!
]style[(12 3 2 4 1 3 4 33 1 20 1 6 4 9)f1b,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 23:09'!
startMotion
	plotsToReach _ path copy.
	moving _ 1.
	self start.
	self compileProgram: path copy!
]style[(11 2 12 3 4 8 6 3 1 3 4 9 4 17 4 5)f1b,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 21:50'!
step
	moving = 0
		ifTrue: [self doNothing].
	moving = 1
		ifTrue: [self moveForward].
	moving = 2
		ifTrue: [self turn]!
]style[(4 2 6 3 1 30 6 3 1 12 4 16 6 3 1 12 4 6)f1b,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 18:43'!
stepTime
	^ 20!
]style[(8 4 2)f1b,f1,f1c172172096! !

!RCXCar methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 20:20'!
stopMotion
	moving _ 0.
	self stopStepping!
]style[(10 2 6 7 4 13)f1b,f1,f1cmagenta;,f1,f1cmagenta;,f1! !

!RCXCar methodsFor: 'motion' stamp: 'AB 1/18/2002 21:42'!
moveForward

| vec |
(self realPosition - plotsToReach first center) r <= 1
				ifTrue: [plotsToReach size = 1
						ifTrue: [self stopMotion.
							^ true].
					plotsToReach removeFirst.
					moving _ 2.
^ false].
			vec _ (plotsToReach first center - self realPosition) normalized.
			self position: self position + vec!
]style[(22 4 16 12 20 1 14 12 8 1 16 4 22 4 8 12 19 6 3 1 4 5 6 3 4 12 16 4 30 4 11 4 12 3)f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cblue;i! !

!RCXCar methodsFor: 'motion' stamp: 'AB 1/18/2002 21:43'!
turn

| t |
t _ plotsToReach first center - self realPosition.
			t _ (t x @ t y negated) degrees.
			t - (90 - self heading) \\ 360 > 180
				ifTrue: [self heading: self heading + 1]
				ifFalse: [self heading: self heading - 1].
			self heading: self heading \\ 360.
			"self halt."
			90 - self heading - t \\ 360 <= 2
				ifTrue: [moving _ 1]!
]style[(6 6 1 3 12 16 4 18 1 4 1 5 1 24 1 4 2 3 4 13 3 3 3 14 4 10 4 11 1 16 4 10 4 11 1 6 4 10 4 12 3 5 12 4 2 3 4 11 1 4 3 4 1 14 6 3 1 1)f1b,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1c122020000,f1,f1c172172096,f1,f1cmagenta;,f1,f1cblue;i,f1,f1c172172096,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096,f1! !


!RCXCar class methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 18:31'!
createWithPath: aPath

	| r |
	r_self new.
	r path: aPath.
^r! !


!RCXCompiler methodsFor: 'RCX specific' stamp: 'AB 1/19/2002 14:14'!
getByteCode

	| r |
	r_OrderedCollection new.

	target do: [:instr| r_r, (self getByteCodeFromInstr: instr first arg: instr allButFirst)].

	^r
		! !

!RCXCompiler methodsFor: 'RCX specific' stamp: 'AB 3/28/2002 21:07'!
getByteCodeFromInstr: instr arg: arg 
	instr = #soundBeep
		ifTrue: [^ RCX new byteCodeForPlaySystemSound: 1].
	instr = #soundClick
		ifTrue: [^ RCX new byteCodeForPlaySystemSound: 0].
	instr = #soundError
		ifTrue: [^ RCX new byteCodeForPlaySystemSound: 4].
	instr = #soundFastSweepUp
		ifTrue: [^ RCX new byteCodeForPlaySystemSound: 5].
	instr = #soundSweepDown
		ifTrue: [^ RCX new byteCodeForPlaySystemSound: 2].
	instr = #soundSweepUp
		ifTrue: [^ RCX new byteCodeForPlaySystemSound: 3].
	instr = #turnOnLeft
		ifTrue: [^ RCX new byteCodeForTurnOnMotor: 1].
	instr = #turnOnMiddle
		ifTrue: [^ RCX new byteCodeForTurnOnMotor: 2].

	instr = #turnOnRight
		ifTrue: [^ RCX new byteCodeForTurnOnMotor: 3].
	instr = #turnOffLeft
		ifTrue: [^ RCX new byteCodeForTurnOffMotor: 1].
	instr = #turnOffMiddle
		ifTrue: [^ RCX new byteCodeForTurnOffMotor: 2].
	instr = #turnOffRight
		ifTrue: [^ RCX new byteCodeForTurnOffMotor: 3].
	instr = #wait
		ifTrue: [^ RCX new byteCodeForWait: 2 arg: (arg first * 100) asInteger].
	instr = #powerMotor
		ifTrue: [^ RCX new
				byteCodeForSetPowerMotor: (2 raisedTo: arg first - 1)
				source: 2
				power: (arg at: 2)].
	self error: 'Instr ' , instr asString , ' unknown !!'! !

!RCXCompiler methodsFor: 'as yet unclassified' stamp: 'AB 3/28/2002 15:51'!
compile: aProgram 
	aProgram
		do: [:instr | target _ target
						, (self getByteCodeFromInstr: instr first arg: instr allButFirst)].
	^ target! !

!RCXCompiler methodsFor: 'as yet unclassified' stamp: 'AB 3/28/2002 15:50'!
compileFromCar: aProgram 
	aProgram
		do: [:instr | target _ target
						, (self getInstrCodeFor: instr first arg: instr allButFirst)].
	^ target! !

!RCXCompiler methodsFor: 'as yet unclassified' stamp: 'AB 1/19/2002 12:29'!
getCompiledProgram

	^target! !

!RCXCompiler methodsFor: 'as yet unclassified' stamp: 'AB 2/17/2002 13:08'!
getInstrCodeFor: instrCode arg: instrArg 
	instrCode = #walk
		ifTrue: [^ Array
				with: #(#turnOnLeft )
				with: #(#turnOnRight )
				with: (Array with: #wait with: instrArg first / vitesse / coeffWalk)
				with: #(#turnOffLeft )
				with: #(#turnOffRight )
				with: #(#wait 1 )].
	instrCode = #turnLeft
		ifTrue: [^ Array
				with: #(#turnOnRight )
				with: (Array with: #wait with: instrArg first / 2 * rayon / vitesse / coeffTurn)
				with: #(#turnOffRight )
				with: #(#wait 1 )].

	instrCode = #turnRight
		ifTrue: [^ Array
				with: #(#turnOnLeft )
				with: (Array with: #wait with: instrArg first / 2 * rayon / vitesse / coeffTurn)
				with: #(#turnOffLeft )
				with: #(#wait 1 )].
	instrCode = #power
		ifTrue: [^ Array
				with: (Array
						with: #powerMotor
						with: 1
						with: instrArg first)
				with: (Array
						with: #powerMotor
						with: 3
						with: instrArg first)].
	self error: 'Instr ' , instrCode asString , ' unknown !!'! !

!RCXCompiler methodsFor: 'as yet unclassified' stamp: 'AB 2/22/2002 20:11'!
initialize
	target _ OrderedCollection new.
	vitesse _ 0.5.
	"Meter/sec"
	rayon _ 0.15.
	"15 centimeters"
	coeffTurn _ 30.
	coeffWalk _ 120! !


!RCXCompiler class methodsFor: 'as yet unclassified' stamp: 'AB 3/28/2002 16:19'!
compile: aProgram 
	| r |
	r _ self new.
	r compile: aProgram.
	^ r getCompiledProgram asByteArray!
]style[(9 8 4 2 4 1 3 4 7 1 10 8 5 1 31)f1b,f1cblue;b,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1! !

!RCXCompiler class methodsFor: 'as yet unclassified' stamp: 'AB 3/28/2002 15:54'!
compileFromCar: aProgram 
	| r |
	r _ self new.
	r compileFromCar: aProgram.
	r getCompiledProgram.
	^ r getByteCode! !

!RCXCompiler class methodsFor: 'as yet unclassified' stamp: 'AB 1/19/2002 14:28'!
getIntermediaire: aProgram
	| r |
	r_self new.
	^r compile: aProgram.
! !

!RCXCompiler class methodsFor: 'as yet unclassified' stamp: 'AB 1/19/2002 12:28'!
new
	^super new initialize! !


!RCXIrEncoder methodsFor: 'read/write with tower' stamp: 'nk 4/6/2002 21:19'!
checkIfWeRepeatTheLastCommandOn: anArrayOfByte
	"If we perform the same command twice in a row, then the last one has to have bit 3 inverted"
	| first |
	first _ anArrayOfByte at: 1.
	lastCommand = first ifTrue: [ anArrayOfByte at: 1 put: (first bitXor: 8) ]! !

!RCXIrEncoder methodsFor: 'read/write with tower' stamp: 'AB 5/5/2002 21:59'!
decode: responseArray 
	| echolen numread message sum msgcount |
	responseArray size = 0
		ifTrue: [^ responseArray].
	lastCommand isNil
		ifTrue: [^ #(255 255 )].
	self checkIfIsCorrect: responseArray.
	echolen _ lastCommandArray size.
	numread _ responseArray size.
	"message _ ByteArray new: numread - (echolen + 3) / 2 - 1."
	message _ ByteArray new: 4096.
	sum _ 0.
	msgcount _ 1.
	echolen + 3
		to: numread - 1
		by: 2
		do: [:loop | 
			self checkIfComplementIsCorrectOn: responseArray at: loop.
			loop < (numread - 2)
				ifTrue: [message
						at: msgcount
						put: (responseArray at: loop + 1).
					msgcount _ msgcount + 1.
					sum _ sum
								+ (responseArray at: loop + 1)]].
	self checkForSumAnswerOn: responseArray with: sum.
	self checkIfCoherentAnswerOn: responseArray.
	((message at: 1)
			bitAnd: 8)
			= 8
		ifTrue: [message
				at: 1
				put: ((message at: 1)
						bitAnd: 247)].
	^ message copyFrom: 1 to: msgcount - 1! !

!RCXIrEncoder methodsFor: 'read/write with tower' stamp: 'AB 1/13/2002 14:56'!
encode: anArrayOfByte

	|rcxArray sum|
	sum_0.
	rcxArray_OrderedCollection new.

	self writePreambleOn: rcxArray.
	self checkIfWeRepeatTheLastCommandOn: anArrayOfByte.

	lastCommand _ anArrayOfByte at: 1.
	anArrayOfByte do: [:b|
		rcxArray add: b; add: (b bitInvert  bitAnd: 16rFF).
		sum_sum+b.
	].
	sum_sum bitAnd: 16rFF.
	rcxArray add: sum; add: (sum bitInvert bitAnd: 16rFF).

	lastCommandArray_rcxArray asArray.

	^rcxArray asByteArray! !

!RCXIrEncoder methodsFor: 'read/write with tower' stamp: 'AB 12/26/2001 21:01'!
writePreambleOn: anOrderedCollection

	anOrderedCollection add: 16r55; add: 16rFF; add: 16r00.! !

!RCXIrEncoder methodsFor: 'initialize' stamp: 'AB 1/13/2002 13:31'!
initialize

	lastCommandArray_'abc' asByteArray. "Dummy !!"! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 16:21'!
checkForSumAnswerOn: responseArray with: sum 
	(self isCorrectSumAnswerOn: responseArray with: sum) ifFalse: [ Transcript show: 'Bad sum for answer !!!!' ;cr ]! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 16:28'!
checkIfCoherentAnswerOn: responseArray 
	"if(responseArray[echolen+3]!!=~lastcommandArray[3])"
	(self isCoherentAnswerOn: responseArray) ifFalse: [Transcript show: 'Incoherent answer !!!!' ;cr]! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 16:13'!
checkIfComplementIsCorrectOn: anArray at: aPosition 
	^ (anArray at: aPosition)
		= ((anArray at: aPosition + 1) negated - 1 + 256)! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 15:47'!
checkIfIsCorrect: anArray 
	(self isCorrectHeader: anArray)
		ifFalse: [Transcript show: '***Error with answer from IR Tower : Header ***';
				 cr].
	anArray size < lastCommandArray size
		ifTrue: [Transcript show: '***Error with answer from IR Tower : Number of byte received is incorrect ***';
				 cr].
	anArray size = lastCommandArray size
		ifTrue: [Transcript show: '***Error with answer from IR Tower : No answer from RCX ***';
				 cr].
	(self isCorrectAnswerSize: anArray)
		ifFalse: [Transcript show: '***Error with answer from IR Tower : bad length ***';
				 cr].
	(self isCorrectHeader: (anArray copyFrom: (lastCommandArray size + 1) to: anArray size ))
		ifFalse: [Transcript show: '***Error with answer from IR Tower : Header ***';
				 cr].! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 16:26'!
isCoherentAnswerOn: responseArray 
	"if(responseArray[echolen+3]!!=~lastcommandArray[3])"
	| echolen |
	echolen _ lastCommandArray size.
	^ (responseArray at: (echolen + 3))
		~= ((lastCommandArray at: 3) negated - 1)! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 16:07'!
isCorrectAnswerSize: anArray 
	| nbRead echolen |
	nbRead _ anArray size.
	echolen _ lastCommandArray size.
	^ (nbRead < (echolen + 5)
			or: [nbRead - echolen - 3 \\ 2 ~= 0])
		ifTrue: [false]
		ifFalse: [true]!
]style[(21 7 4 15 4 6 3 7 8 7 3 16 11 6 4 7 3 2 9 6 3 7 3 1 4 1 4 1 14 5 14 4 1)f1b,f1cblue;b,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cblue;i,f1,f1c200200124,f1,f1cblue;i,f1,f1cblue;i,f1,f1c200200124,f1,f1c200200124,f1,f1c200200124,f1,f1cmagenta;,f1,f1cmagenta;,f1! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 15:27'!
isCorrectHeader: anArray 
	^ anArray size < 3
		ifTrue: [false]
		ifFalse: [(anArray copyFrom: 1 to: 3)
				= (ByteArray
						with: 85
						with: 255
						with: 0)]! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 5/5/2002 17:14'!
isCorrectSumAnswerOn: responseArray with: sum 
	^ (responseArray at: responseArray size - 1)
		= (sum \\ 256)!
]style[(22 13 7 3 6 13 5 13 8 1 7 3 4 3 1)f1b,f1cblue;b,f1b,f1cblue;b,f1,f1cblue;i,f1,f1cblue;i,f1,f1c200200124,f1,f1cblue;i,f1,f1c200200124,f1! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 3/15/2002 14:37'!
log: anObject 
	Transcript show: anObject asString! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 1/15/2002 16:42'!
setLastCommandArray: aByteArray

	lastCommandArray_aByteArray! !

!RCXIrEncoder methodsFor: 'test' stamp: 'AB 1/13/2002 13:15'!
testEncode
	"Test the encode method"

	|rcx array res|
	rcx_RCXIrEncoder new.
	array_ByteArray new: 2.

	array at: 1 put: 16rE1. "Set motor A direction forward"
	array at: 2 put: 16r81.

	res_(rcx encode: array).
"85 -1 0 -31 30 -127 126 98 -99"
	^(res = #(85 255 0 225 30 129 126 98 157)).
! !


!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 21:51'!
backward
	| v |
	direction _ false.
	v _ RCXMotor getByteCodeFromMotor: plug motorStatus: 0.
	^((rcx
		sendAndRead: (ByteArray with: 225 with: v)) at: 1) = 22
! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 21:52'!
float
	| v t |
	v _ RCXMotor getByteCodeFromMotor: plug motorStatus: 0.
	t _ ByteArray new: 2.
	t at: 1 put: 33.
	t at: 2 put: v.
	^((rcx sendAndRead: t) at: 1) = 214
! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 21:52'!
forward
	| v |
	direction _ true.
	v _ RCXMotor getByteCodeFromMotor: plug motorStatus: 2.
	^((rcx
		sendAndRead: (ByteArray with: 225 with: v)) at: 1) = 22
! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 20:10'!
initialize

	"Default direction is forward"
	direction_true.
	started _ false.! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 1/15/2002 16:13'!
isBackward

	^direction = false.! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 1/15/2002 16:13'!
isForward

	^direction = true.! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 20:11'!
isStarted
	^started! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 1/13/2002 15:51'!
onPlug: aNumber rcx: aRCX

	plug_aNumber.
	rcx_aRCX.! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 20:05'!
plug
	^plug! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 21:53'!
reverse
	| v |
	direction _ direction not.
	v _ RCXMotor getByteCodeFromMotor: plug motorStatus: 1.
	^((rcx
		sendAndRead: (ByteArray with: 225 with: v)) at: 1) = 22
! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 21:53'!
start
	| v |
	v _ RCXMotor getByteCodeFromMotor: plug motorStatus: 2.
	started _ ((rcx
		sendAndRead: (ByteArray with: 33 with: v)) at: 1) = 214.
	^ started
! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 21:54'!
stop
	| v t |
	v _ RCXMotor getByteCodeFromMotor: plug motorStatus: 1.
	t _ ByteArray new: 2.
	t at: 1 put: 33.
	t at: 2 put: v.
	started _ (((rcx sendAndRead: t) at: 1) = 214) not.

	^ started not! !


!RCXMotor class methodsFor: 'as yet unclassified' stamp: 'AB 1/13/2002 17:07'!
getByteCodeFromMotor: aNumber motorStatus: aStatus
	"aNumber between 1 and 3
	 aStatus : 	0 float
				1 off
				2 on"

	^ (2 raisedTo: (aNumber -1)) + (aStatus * 64) ! !

!RCXMotor class methodsFor: 'as yet unclassified' stamp: 'AB 1/15/2002 15:55'!
getDirectionByteCodeFromMotor: aNumber direction: aStatus
	"aNumber between 1 and 3
	 aStatus : 	0 backward
				1 reverse
				2 forward"

	^ (2 raisedTo: (aNumber -1)) + (aStatus * 64) ! !

!RCXMotor class methodsFor: 'as yet unclassified' stamp: 'AB 1/15/2002 15:59'!
new

	^super new initialize! !

!RCXMotor class methodsFor: 'as yet unclassified' stamp: 'AB 1/13/2002 15:37'!
onPlug: aNumber rcx: aRCX

	^self new onPlug: aNumber rcx: aRCX .

! !


!RCXPathGenerator methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 15:22'!
initialize
	super initialize.
	value _ 0.
	self label: 'Get new Checkpoint'!
]style[(10 2 5 14 5 3 1 3 4 8 20)f1b,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096! !

!RCXPathGenerator methodsFor: 'as yet unclassified' stamp: 'ab 7/31/2002 13:50'!
mouseDown: evt 
	super mouseDown: evt.
	(RCXPlot createWithValue: value) openInHand.
	value_value+1.!
]style[(11 3 3 5 12 66)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cblue;i! !


!RCXPlot methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 15:06'!
drawOn: aCanvas 
	super drawOn: aCanvas.
	value notNil
		ifTrue: [aCanvas
				text: ' ',value asString
				at: self position
				font: nil
				color: Color black]!
]style[(8 7 3 5 9 7 3 5 19 7 15 5 18 4 20 3 12 5 7)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cblue;i,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cmagenta;,f1! !

!RCXPlot methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 14:27'!
initialize
	super initialize.

	self width: 16.
	self height: 16!
]style[(10 22 4 8 2 3 4 9 2)f1b,f1,f1cmagenta;,f1,f1c172172096,f1,f1cmagenta;,f1,f1c172172096! !

!RCXPlot methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 14:21'!
value
	^value! !

!RCXPlot methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 14:21'!
value: aNumber
	value_aNumber! !


!RCXPlot class methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 14:47'!
createWithValue: aNumber 
	^self new value: aNumber!
]style[(17 7 4 4 12 7)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cblue;i! !


!RCXSerialPortReader methodsFor: 'communication' stamp: 'nk 4/6/2002 14:27'!
flush
	sharedQueue flush.
	serialPort readByteArray.! !

!RCXSerialPortReader methodsFor: 'communication' stamp: 'nk 4/6/2002 13:11'!
nextPutAll: aStringOrByteArray 
	self logSentBytes: aStringOrByteArray.
	serialPort nextPutAll: aStringOrByteArray! !

!RCXSerialPortReader methodsFor: 'communication' stamp: 'nk 4/6/2002 14:58'!
readByteArray
	| delay stream bytes lastReceived |
	stream _ WriteStream on: (ByteArray new: 80).
	delay _ Delay forMilliseconds: 100.
	lastReceived _ Time millisecondClockValue.
	"Poll the receive queue until there have been no characters for at least 600 msec"
	[
		delay wait.
		bytes _ sharedQueue nextOrNil.
		bytes ifNotNil: [
			stream nextPutAll: bytes.
			lastReceived _ Time millisecondClockValue.
			true
		]
		ifNil: [ (Time millisecondClockValue - lastReceived) < 300 ]
	] whileTrue.
	bytes _ stream contents.
	self logReceivedBytes: bytes.
	^bytes
					! !

!RCXSerialPortReader methodsFor: 'logging' stamp: 'nk 4/6/2002 15:05'!
enableLogging: aBoolean
	logging _ aBoolean! !

!RCXSerialPortReader methodsFor: 'logging' stamp: 'nk 4/6/2002 14:45'!
logDirection: dir andBytes: string
	WorldState addDeferredUIMessage: [
	Transcript nextPutAll: dir;
		nextPutAll: string size printString;
		nextPutAll: ' bytes: '.
		string do: [ :ch | Transcript
			nextPutAll: (Character value: ch) hex;
			space ].
		Transcript cr; flush
	]! !

!RCXSerialPortReader methodsFor: 'logging' stamp: 'nk 4/6/2002 15:07'!
logReceivedBytes: string
	logging ifTrue: [ self logDirection: 'RECV ' andBytes: string ]! !

!RCXSerialPortReader methodsFor: 'logging' stamp: 'nk 4/6/2002 15:07'!
logSentBytes: string
	logging ifTrue: [ self logDirection: 'SEND ' andBytes: string ]! !

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:13'!
close
	self stopReading.
	serialPort ifNotNil: [ serialPort close ].
! !

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'AB 5/5/2002 15:03'!
initialize
	sharedQueue _ SharedQueue new.
	logging _ true! !

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:08'!
openPort: number 
	portNumber _ number.
	serialPort _ (SerialPort new)
		baudRate: 2400;
		dataBits: 8;
		parityType: 1;	"odd parity"
		stopBitsType: 1;
		openPort: portNumber.
! !

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:07'!
reopen
	self openPort: portNumber.
	self startReading.
! !

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:04'!
startReading
	serialPort readByteArray.	"flush it"
	process _ [ | delay |
		delay _ (Delay forMilliseconds: 100).
		[ | bytes | 
			bytes _ serialPort readByteArray.
			(bytes notNil and: [ bytes notEmpty ])
				ifTrue: [ sharedQueue nextPut: bytes].
			delay wait ] repeat
		] forkAt: Processor userInterruptPriority ! !

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:05'!
stopReading
	process ifNotNil: [
		process terminate.
		process _ nil.
	].
! !

!RCXSerialPortReader methodsFor: 'accessing' stamp: 'nk 4/6/2002 21:25'!
portNumber
	^portNumber! !


!RCXSerialPortReader class methodsFor: 'as yet unclassified' stamp: 'AB 2/23/2002 21:22'!
createFromSerialPort: aSerialPort
	^self new initialize serialPort: aSerialPort! !

!RCXSerialPortReader class methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 16:06'!
onPort: number 
	^self new initialize openPort: number; startReading! !


!RCXTest methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 16:09'!
replyForAlive
	"55 FF 00 18 E7 18 E7 55 FF 00 E7 18 E7 18"
	^ #(16r55 16rFF 16r00 16r18 16rE7 16r18 16rE7 16r55 16rFF 16r00 16rE7 16r18 16rE7 16r18) asByteArray! !

!RCXTest methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 16:14'!
sendForAlive
	"55 FF 00 18 E7 18 E7"
	^ #(16r55 16rFF 16r00 16r18 16rE7 16r18 16rE7) asByteArray! !

!RCXTest methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 16:14'!
testCheckComplent
	| r t |
	r _ RCXIrEncoder new.
	t _ self sendForAlive.
	4
		to: t size - 1
		by: 2
		do: [:i | self
				assert: (r checkIfComplementIsCorrectOn: t at: i)]! !

!RCXTest methodsFor: 'as yet unclassified' stamp: 'AB 5/5/2002 15:31'!
testCheckHeader
	| r |
	r _ RCXIrEncoder new.
	self
		assert: (r isCorrectHeader: self replyForAlive)! !


!RCXTouchSensor class methodsFor: 'as yet unclassified' stamp: 'AB 1/15/2002 16:22'!
onPlug: aNumber rcx: aRCX

	! !


!RCXWorld methodsFor: 'as yet unclassified' stamp: 'AB 2/16/2002 17:32'!
createCarWith: plotsList 
	| myCar |
	myCar _ RCXCar createWithPath: plotsList.
	self addMorph: myCar.
	myCar initPosition: (plotsList at: 1) center.
	myCar directionOfPoint: (plotsList at: 2) center.
	myCar startMotion.
	^ myCar!
]style[(15 9 4 6 4 5 3 6 17 9 3 4 13 3 3 5 16 9 5 1 11 5 20 9 5 1 11 5 17 5)f1b,f1cblack;b,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;,f1,f1cblue;,f1,f1cblack;,f1,f1cblack;! !

!RCXWorld methodsFor: 'as yet unclassified' stamp: 'AB 1/18/2002 15:28'!
drawOn: aCanvas

	super drawOn: aCanvas.
	plots notNil ifTrue: [ self drawPathOn: aCanvas].! !

!RCXWorld methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 22:11'!
drawPathOn: aCanvas

	plots allButLast
		with: plots allButFirst
		do: [:a :b | (aCanvas
				line: a center
				to: b center
				
				width: 3 color: Color red).
				aCanvas text: (a center - b center) r asInteger asString
						at: ((a center + b center) / 2) asIntegerPoint
						font: nil color: Color blue   ]!
]style[(19 3 5 20 5 20 6 21 1 16 1 24 2 7 5 158)f1b,f1,f1cmagenta;,f1,f1cmagenta;,f1,f1cred;,f1,f1cblue;i,f1,f1cblue;i,f1,f1c170170092,f1,f1cmagenta;,f1! !

!RCXWorld methodsFor: 'as yet unclassified' stamp: 'AB 2/16/2002 17:18'!
initialize
	super initialize.
	value _ 0.
	self extent: 400 @ 300.
	self layout.! !

!RCXWorld methodsFor: 'Callback' stamp: 'AB 4/30/2002 15:18'!
downloadToRCX
	RCX
		downloadRawProgram: (RCXCompiler compileFromCar: car getProgram)
		as: 1 onPort: 1.! !

!RCXWorld methodsFor: 'Callback' stamp: 'AB 2/16/2002 17:33'!
getProgram
	car getProgram inspect! !

!RCXWorld methodsFor: 'Callback' stamp: 'AB 2/16/2002 17:28'!
initMotion
	plots _ self submorphs
				select: [:p | p isKindOf: RCXPlot].
	plots _ plots
				asSortedCollection: [:a :b | a value < b value].
	car ifNotNil: [car delete. car_nil].
	car_self createCarWith: plots! !

!RCXWorld methodsFor: 'Callback' stamp: 'AB 2/16/2002 17:35'!
layout
	| pathGenerator initButton downloadButton getProgramButton |
	pathGenerator _ RCXPathGenerator new.
	initButton _ RCXWorldButton getInitMotion.
	downloadButton _ RCXWorldButton getDownloadToRCX.
	getProgramButton _ RCXWorldButton getProgram.
	pathGenerator position: 5 @ 5.
	initButton position: 5 @ 25.
	getProgramButton position: 5@ 45.
	downloadButton position: 5 @ 65.
	self addMorph: pathGenerator;
		 addMorph: initButton;
		 addMorph: getProgramButton;
		 addMorph: downloadButton! !


!RCXWorld class methodsFor: 'as yet unclassified' stamp: 'AB 3/15/2002 14:43'!
open
	"RCXWorld open"
	self new openInWorld! !


!RCXWorldButton methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 14:52'!
createLabel: aString action: aSymbol

	self setLabel: aString.
	self setAction: aSymbol! !

!RCXWorldButton methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 14:49'!
mouseDown: evt 
	super mouseDown: evt.

	(self owner isKindOf: RCXWorld)
		ifTrue: [self owner perform: action]!
]style[(11 3 3 5 12 77)f1b,f1cblue;b,f1,f1cmagenta;,f1,f1cblue;i! !

!RCXWorldButton methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 14:51'!
setAction: aSymbol

	action_aSymbol! !

!RCXWorldButton methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 14:51'!
setLabel: aString

	self label: aString! !


!RCXWorldButton class methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 14:55'!
createLabel: aString action: aSymbol

	^self new createLabel: aString action: aSymbol! !

!RCXWorldButton class methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 15:05'!
getDownloadToRCX

	^self createLabel: 'Download' action: #downloadToRCX! !

!RCXWorldButton class methodsFor: 'as yet unclassified' stamp: 'AB 1/20/2002 14:55'!
getInitMotion

	^self createLabel: 'InitMotion' action: #initMotion! !

!RCXWorldButton class methodsFor: 'as yet unclassified' stamp: 'AB 2/16/2002 17:33'!
getProgram
	^ self createLabel: 'Get Program' action: #getProgram! !


!UndefinedObject methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:50'!
additionsToViewerCategories
	"Answer viewer additions for the 'mindstorms' category"

	^#((
		Mindstorms 
		(
			(slot motorASpeed 'Motor A Speed' Number readWrite Player getMotorASpeed Player setMotorASpeed:)
			(slot motorBSpeed 'Motor B Speed' Number readWrite Player getMotorBSpeed Player setMotorBSpeed:)
			(slot motorCSpeed 'Motor C Speed' Number readWrite Player getMotorCSpeed Player setMotorCSpeed:)
		)))
! !

!UndefinedObject methodsFor: 'instance creation' stamp: 'nk 4/6/2002 20:59'!
authoringPrototype
	^self onPort: 0! !

!UndefinedObject methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:49'!
descriptionForPartsBin
	"Answer a description for use in parts bins"

	^ self partName: 	'RCX Brick'
		categories:		#('Mindstorms')
		documentation:	'A scriptable RCX brick that connects via your IR tower'! !

!UndefinedObject methodsFor: 'instance creation' stamp: 'nk 4/6/2002 18:03'!
initialize
	"RCXMorph initialize"
	[
		RCXImage _ ImageReadWriter formFromFileNamed: 'rcx2small.gif'
	] on: FileDoesNotExistException do: [ :ex | RCXImage _ nil ].
	RCXImage ifNil: [ self inform: 'can''t open file rcx2small.gif' ]! !

!UndefinedObject methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:48'!
newStandAlone
	^self onPort: 0! !

!UndefinedObject methodsFor: 'instance creation' stamp: 'nk 4/6/2002 19:59'!
onPort: port
	^super new initializeOnPort: port! !


!RCXSerialPortReader reorganize!
('communication' flush nextPutAll: readByteArray)
('logging' enableLogging: logDirection:andBytes: logReceivedBytes: logSentBytes:)
('initialize-release' close initialize openPort: reopen startReading stopReading)
('accessing' portNumber)
!


!RCX class reorganize!
('Examples' downloadProgram:as: downloadProgram:as:onPort: downloadRawProgram:as:onPort: example1)
('initialize-release' activeRCXs shutDown startUp)
('instance creation' onPort:)
('utilities' getChecksum: log:)
('as yet unclassified' new)
!


!RCX reorganize!
('ByteCode generator' byteCodeForClearTimer: byteCodeForPlaySystemSound: byteCodeForSelectProgram: byteCodeForSetPowerMotor:source:power: byteCodeForStartTaskDownloadingWithTask:andLength: byteCodeForTransfertForIndex:data: byteCodeForTurnOffMotor: byteCodeForTurnOnMotor: byteCodeForWait:arg:)
('accessing' portNumber)
('initialize-release' initialize)
('private' closePort doInitAlive initializeOnPort: log: reopenPort shutDownPort)
('operation' enableLogging: isAlive motor: sound: soundBeep soundClick soundError soundFastSweepUp soundSweepDown soundSweepUp touchSensor:)
('transferring' downloadRawProgram:as: flush read selectProgram: send: sendAndRead: startTaskDownloadingWithTask:andLength: transfertForIndex:data:)
('as yet unclassified' onPort:)
!

