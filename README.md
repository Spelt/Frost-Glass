# Frost Glass Control

- STILL IN BETA -

A little pet project ;-) It's a Firemonkey component which is frost glass like component just like the IOS 8/10 popup sound control and IOS Control Centre (bottom slide up).

which is able to blur and color tinting. 
It is a decendent of the very versatile TRectangle and thus also support borders and round corners.

### About the project
Performance is utterly important here, it was/is a challenge to get this component as smooth as possible.  If you think you can shave off more milli seconds? Let me know!

In some cases it is wise to use the build in cache feature and somtimes it's not (changing backgrounds or with slide pop ins you want continue updates). Anyway you can test for yourself via the included test project. Just resize the form at runtime to see the speed result in ms in the lisbox via the onTickEvent. I added a build-in TStopwatch to measure performance.


The inspiration for this component are the IOS 8/10 popup sound controls and IOS Control Centre (bottom slide up). Code inspiration is from: http://stackoverflow.com/questions/23898849/ios7-blurred-overlay-in-delphi-xe6


###Licence
Frost Glass is released under the Apache 2.0 license.  A copy of the Apache 2.0 license can be found here: http://www.apache.org/licenses/LICENSE-2.0



###Changes
- vBeta 1 Date: 2017/03/05 
	- Initial upload to github



###Features
- Demo project

Properties available
- Blur (Gaussian) enabled 
- FrostColor
- FrostColor Enabled
- Caching


Important inherited properties of TRectangle which will make it look more great.
- Border functionality
- Corner functionality

