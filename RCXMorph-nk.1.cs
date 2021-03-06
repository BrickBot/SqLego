'From Squeak3.2 of 11 July 2002 [latest update: #4952] on 19 August 2002 at 12:23:20 pm'!
"Change Set:		RCXMorph-nk
Date:			19 August 2002
Author:			Ned Konz

This change set builds on Alexandre Bergel's RCX
change set, available from the Squeak Swiki
at http://minnow.cc.gatech.edu/squeak/2412

You must also load the RCXFixes-nk change set FIRST.

This fixes some communications problems in
Alexandre Bergel's rcx change set, and adds a
scriptable RCXMorph. You can click on the
A, B, or C connectors on the Morph image to
turn the motors on and off.

It also allows multiple users of a single RCX connection,
and attempts to re-connect at image startup time.

You must have the image file rcx2small.gif in
your Squeak directory before you load this change set.

Only the motors and sounds work so far."

(FileDirectory default fileExists: 'rcx2small.gif')
ifFalse: [ self error: 'you must have rcx2small.gif in your Squeak directory first' ]!

Morph subclass: #RCXMorph
	instanceVariableNames: 'rcx '
	classVariableNames: 'RCXImage '
	poolDictionaries: ''
	category: 'People-nk-RCXMorph'!

!EToyVocabulary methodsFor: 'initialization' stamp: 'nk 8/19/2002 11:30'!
morphClassesDeclaringViewerAdditions
	"Answer a list of actual morph classes implementing #additionsToViewerCategories"

	| survivors |

	survivors _ OrderedCollection new.
	(Smalltalk allImplementorsOf: #additionsToViewerCategories) do: [ :aMarker |
		(aMarker actualClass isMeta and: [ (aMarker actualClass soleInstance isKindOf: Morph class)]) ifTrue: [
			survivors add: aMarker actualClass soleInstance
		]
	].
	^ survivors
"EToyVocabulary basicNew morphClassesDeclaringViewerAdditions"! !


!Player methodsFor: 'slot getters/setters' stamp: 'nk 4/6/2002 21:06'!
getMotorASpeed
	^  self getValueFromCostume: #getMotorASpeed! !

!Player methodsFor: 'slot getters/setters' stamp: 'nk 4/6/2002 21:06'!
getMotorBSpeed
	^  self getValueFromCostume: #getMotorBSpeed! !

!Player methodsFor: 'slot getters/setters' stamp: 'nk 4/6/2002 21:06'!
getMotorCSpeed
	^  self getValueFromCostume: #getMotorCSpeed! !

!Player methodsFor: 'slot getters/setters' stamp: 'nk 4/6/2002 21:07'!
setMotorASpeed: aNumber
	costume setMotorASpeed: aNumber! !

!Player methodsFor: 'slot getters/setters' stamp: 'nk 4/6/2002 21:07'!
setMotorBSpeed: aNumber
	costume setMotorBSpeed: aNumber! !

!Player methodsFor: 'slot getters/setters' stamp: 'nk 4/6/2002 21:07'!
setMotorCSpeed: aNumber
	costume setMotorCSpeed: aNumber! !


!RCXMorph methodsFor: 'initialization' stamp: 'nk 4/6/2002 21:30'!
initializeOnPort: port
	super initialize.
	self
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	{ $1 -> (16@34) . $2 -> (49@34) . $3 -> (82@34) .
	$A -> (16@117) . $B -> (49@117) . $C -> (82@117) }
	do: [ :assoc | | button |
		button _ (RectangleMorph new)
			borderWidth: 0;
			color:  (Color transparent);
			bounds: (assoc value extent: 33@33).
		button setNamePropertyTo: (String with: $I with: $O with: assoc key).
		self addMorphBack: button.
		assoc key isLetter ifTrue: [
			button on: #mouseDown send: #toggleMotor:event:sourceMorph: to: self withValue: assoc key
		]
	].
	RCXImage ifNotNil: [ self addMorphBack: RCXImage asMorph ].

	rcx _ RCX onPort: port.

	self setBalloonText: 'click on my A, B, or C connectors to turn my motors on and off'.
	^self.! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 21:09'!
getMotorASpeed
	| motor |
	motor _ rcx motor: 1.
	^motor isStarted ifFalse: [ 0 ] ifTrue: [ motor isForward ifTrue: [ 1 ] ifFalse: [ -1 ]]! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 21:09'!
getMotorBSpeed
	| motor |
	motor _ rcx motor: 2.
	^motor isStarted ifFalse: [ 0 ] ifTrue: [ motor isForward ifTrue: [ 1 ] ifFalse: [ -1 ]]! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 21:09'!
getMotorCSpeed
	| motor |
	motor _ rcx motor: 3.
	^motor isStarted ifFalse: [ 0 ] ifTrue: [ motor isForward ifTrue: [ 1 ] ifFalse: [ -1 ]]! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 21:11'!
setMotorASpeed: aNumber
	| motor |
	motor _ rcx motor: 1.
	aNumber isZero ifTrue: [ motor stop ]
		ifFalse: [ aNumber > 0
				ifTrue: [ motor forward  ]
				ifFalse: [ motor backward ].
			motor start
		]! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 21:12'!
setMotorBSpeed: aNumber
	| motor |
	motor _ rcx motor: 2.
	aNumber isZero ifTrue: [ motor stop ]
		ifFalse: [ aNumber > 0
				ifTrue: [ motor forward  ]
				ifFalse: [ motor backward ].
			motor start
		]! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 21:12'!
setMotorCSpeed: aNumber
	| motor |
	motor _ rcx motor: 3.
	aNumber isZero ifTrue: [ motor stop ]
		ifFalse: [ aNumber > 0
				ifTrue: [ motor forward  ]
				ifFalse: [ motor backward ].
			motor start
		]! !

!RCXMorph methodsFor: 'operation' stamp: 'nk 4/6/2002 20:47'!
toggleMotor: motorCode event: ev sourceMorph: button
	| motor |
	motor _  rcx motor: ((motorCode asciiValue) - ($A asciiValue) + 1).
	motor isStarted ifTrue: [
		button color: (Color transparent).
		motor stop.
	]
	ifFalse: [
		button color: (TranslucentColor r: 0.972 g: 0.156 b: 0.568 alpha: 0.537).
		motor start.
	]! !


!RCXMorph class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:50'!
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

!RCXMorph class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 20:59'!
authoringPrototype
	^self onPort: 0! !

!RCXMorph class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:49'!
descriptionForPartsBin
	"Answer a description for use in parts bins"

	^ self partName: 	'RCX Brick'
		categories:		#('Mindstorms')
		documentation:	'A scriptable RCX brick that connects via your IR tower'! !

!RCXMorph class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 18:03'!
initialize
	"RCXMorph initialize"
	[
		RCXImage _ ImageReadWriter formFromFileNamed: 'rcx2small.gif'
	] on: FileDoesNotExistException do: [ :ex | RCXImage _ nil ].
	RCXImage ifNil: [ self inform: 'can''t open file rcx2small.gif' ]! !

!RCXMorph class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 21:48'!
newStandAlone
	^self onPort: 0! !

!RCXMorph class methodsFor: 'instance creation' stamp: 'nk 4/6/2002 19:59'!
onPort: port
	^super new initializeOnPort: port! !

UndefinedObject removeSelector: #additionsToViewerCategories!
RCXMorph initialize!
