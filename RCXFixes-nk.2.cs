'From Squeak3.2 of 11 July 2002 [latest update: #4952] on 19 August 2002 at 12:22:54 pm'!
"Change Set:		RCXFixes-nk
Date:			19 August 2002
Author:			Ned Konz <ned@bike-nomad.com>

19 Aug: updated for 3.2.

Made serial comms work more reliably.
Removed most delays.
Added switchable byte logging, made logging happen in UI thread.
"!

Object subclass: #RCX
	instanceVariableNames: 'port irEncoder motorsList '
	classVariableNames: 'RCXsActive '
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
Object subclass: #RCXMotor
	instanceVariableNames: 'plug rcx direction started '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!
Object subclass: #RCXSerialPortReader
	instanceVariableNames: 'sharedQueue serialPort process logging portNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'People-Ab-RCX'!

!RCX methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:11'!
closePort
	port close.
	self class activeRCXs remove: self!
]style[(9 2 4 43)f1b,f1,f1cblack;,f1! !

!RCX methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 16:12'!
initialize

	motorsList _ OrderedCollection new.! !

!RCX methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 12:49'!
onPort: portNumber 
	port _ RCXSerialPortReader onPort: portNumber.
	irEncoder _ RCXIrEncoder new.
	self doInitAlive! !

!RCX methodsFor: 'private' stamp: 'nk 8/19/2002 11:44'!
doInitAlive
	| r |
	r_#(85 255 0 16 239 16 239) asByteArray.
	port nextPutAll: r.
	irEncoder setLastCommandArray: r.
	r _ self read.
	^ r isEmpty not and: [ (r at: 1) = 231 ]! !

!RCX methodsFor: 'private' stamp: 'nk 4/6/2002 21:29'!
initializeOnPort: portNumber 
	motorsList _ OrderedCollection new.
	port _ RCXSerialPortReader onPort: portNumber.
	irEncoder _ RCXIrEncoder new.
	self doInitAlive! !

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

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 14:53'!
downloadRawProgram: byteCodes as: aNumber
	self log: (self selectProgram: aNumber).
	self log: (self startTaskDownloadingWithTask: 0 andLength: byteCodes size) asArray.
	self log: (self transfertForIndex: 0 data: byteCodes) asArray.
! !

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 13:45'!
flush
	port flush! !

!RCX methodsFor: 'transferring' stamp: 'nk 8/19/2002 11:43'!
read
	^ irEncoder decode: port readByteArray ! !

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 14:14'!
send: aByteArray 
	| r |
	r _ irEncoder encode: aByteArray.
	port nextPutAll: r.
! !

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 12:38'!
startTaskDownloadingWithTask: taskNumber andLength: taskLength 
	self send: (self byteCodeForStartTaskDownloadingWithTask: taskNumber andLength: taskLength).
	^ self read !
]style[(30 10 12 10 3 4 54 10 12 10 6 4 6)f1b,f1cblack;b,f1b,f1cblack;b,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1,f1cblack;,f1! !

!RCX methodsFor: 'transferring' stamp: 'nk 4/6/2002 13:07'!
transfertForIndex: index data: aProgram 
	self
		send: (self byteCodeForTransfertForIndex: index data: aProgram).
	^ self read! !


!RCX class methodsFor: 'Examples' stamp: 'nk 4/6/2002 21:33'!
downloadRawProgram: byteCodes as: aNumber onPort: aPortNumber 
	| rcx |
	rcx _ self onPort: aPortNumber.
	rcx isAlive.
	self log: (rcx selectProgram: aNumber).
	self log: (rcx startTaskDownloadingWithTask: 0 andLength: byteCodes size) asArray.
	self log: (rcx transfertForIndex: 0 data: byteCodes) asArray.
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

!RCX class methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 15:59'!
new
	| newMe |
	newMe _ super new initialize.
	self activeRCXs add: newMe.
	^newMe! !


!RCXMotor methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 21:37'!
float

	| v t |
	v_RCXMotor getByteCodeFromMotor: plug motorStatus: 0.

	t_ByteArray new: 2.
	t at: 1 put: 16r21.
	t at: 2 put: v.

	rcx send: t.

	^ ((rcx read) at: 1) = 16rD6!
]style[(7 159 1 9)f1b,f1,f1cblue;,f1! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'nk 8/19/2002 12:22'!
start
	| v response |
	v_RCXMotor getByteCodeFromMotor: plug motorStatus: 2.

	rcx send: (ByteArray with: 16r21 with: v).
	response _ rcx read.
	^ started _ response isEmpty not and: [ (response at: 1) = 16rD6 ]!
]style[(5 194 1 11)f1b,f1,f1cblue;,f1! !

!RCXMotor methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 20:11'!
stop

	|v t|
	v_RCXMotor getByteCodeFromMotor: plug motorStatus: 1.

	t_ByteArray new: 2.
	t at: 1 put: 16r21.
	t at: 2 put: v.

	rcx send: t.

	started _ (((rcx read) at: 1) = 16rD6) not.
	^ started not! !


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

!RCXSerialPortReader methodsFor: 'initialize-release' stamp: 'nk 4/6/2002 15:07'!
initialize
	sharedQueue _ SharedQueue new.
	logging _ false.! !

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


!RCXSerialPortReader class methodsFor: 'as yet unclassified' stamp: 'nk 4/6/2002 16:06'!
onPort: number 
	^self new initialize openPort: number; startReading! !


!RCXSerialPortReader reorganize!
('communication' flush nextPutAll: readByteArray)
('logging' enableLogging: logDirection:andBytes: logReceivedBytes: logSentBytes:)
('initialize-release' close initialize openPort: reopen startReading stopReading)
('accessing' portNumber)
!


!RCX reorganize!
('ByteCode generator' byteCodeForClearTimer: byteCodeForPlaySystemSound: byteCodeForSelectProgram: byteCodeForSetPowerMotor:source:power: byteCodeForStartTaskDownloadingWithTask:andLength: byteCodeForTransfertForIndex:data: byteCodeForTurnOffMotor: byteCodeForTurnOnMotor: byteCodeForWait:arg:)
('initialize-release' closePort initialize onPort:)
('private' doInitAlive initializeOnPort: log: reopenPort shutDownPort)
('operation' enableLogging: isAlive motor: sound: soundBeep soundClick soundError soundFastSweepUp soundSweepDown soundSweepUp touchSensor:)
('transferring' downloadRawProgram:as: flush read selectProgram: send: sendAndRead: startTaskDownloadingWithTask:andLength: transfertForIndex:data:)
('accessing' portNumber)
!

