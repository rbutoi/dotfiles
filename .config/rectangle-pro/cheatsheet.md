# Rectangle Pro cheat sheet

Generated from `~/Library/Preferences/com.knollsoft.Hookshot.plist` (Rectangle Pro
ships under the bundle id `com.knollsoft.Hookshot` — "Hookshot" is the app's old
name). `~/Library/Application Support/Rectangle Pro/` holds only Paddle license
files, no bindings.

The plist is a binary plist and is **not** tracked in this repo. `iCloudSync` is
on, so bindings also live in iCloud and can be rewritten from another Mac.

To re-read it:

```fish
plutil -convert xml1 -o - ~/Library/Preferences/com.knollsoft.Hookshot.plist
```

## Keyboard shortcuts

11 of 134 actions are bound. Two modifier clusters, no overlap between them.

### ⌃⌘ — reshape the focused window

| Keys | Action | What it does |
| --- | --- | --- |
| `⌃⌘H` | Fill Left | Fill the left side |
| `⌃⌘L` | Fill Right | Fill the right side |
| `⌃⌘J` | Larger Height | Grow vertically |
| `⌃⌘K` | Smaller Height | Shrink vertically |
| `⌃⌘=` | Larger | Grow both dimensions |
| `⌃⌘-` | Smaller | Shrink both dimensions |
| `⌃⌘T` | Tidy | Clean up / align the window |
| `⌃⌘⌫` | Restore | Back to the pre-Rectangle frame |

### ⇧⌘ — recall and displays

| Keys | Action | What it does |
| --- | --- | --- |
| `⇧⌘↑` | Maximize Height | Full height, keep width |
| `⇧⌘M` | Previous Display | Throw to the display before this one |
| `⇧⌘⌫` | Last | Re-apply the most recent action |

### Mnemonics

- `H` / `L` — vim horizontal, fill left / right.
- `J` / `K` — vim vertical, height bigger / smaller.
- `=` / `-` — the usual zoom pair, whole window.
- `⌫` — the two "go back" actions, split by modifier: `⌃⌘⌫` Restore, `⇧⌘⌫` Last.

Note `⌃⌘J`/`⌃⌘K` change height only; there is no bound width equivalent
(`largerWidth` / `smallerWidth` are free).

## Non-keyboard bindings

Also configured in the same plist, for completeness:

- **Drag-to-snap** (`windowSnapping`) on, with 7 of 8 landscape edge/corner zones
  assigned (`landscapeSnapAreas`). The bottom edge is unassigned.
- **Window modifier** `⌃⌘` (`winModFlags`) — same cluster as the shortcuts above.
- **Gesture modifier** `⌥⇧` (`gestures`), one gesture bound.
- **Reticle** enabled with all 8 directions assigned (`mainReticleSpec`), tinted
  orange at 50% alpha.
- Haptic feedback on snap, footprint animation at 0.75×.

## Unbound actions

The other 123 actions, grouped — everything below is currently free to bind.

**Halves & fractions** (30)
`leftHalf`, `rightHalf`, `topHalf`, `bottomHalf`, `centerHalf`, `firstThird`,
`centerThird`, `lastThird`, `firstTwoThirds`, `centerTwoThirds`, `lastTwoThirds`,
`topVerticalThird`, `middleVerticalThird`, `bottomVerticalThird`,
`topVerticalTwoThirds`, `bottomVerticalTwoThirds`, `firstFourth`, `secondFourth`,
`thirdFourth`, `lastFourth`, `firstThreeFourths`, `centerThreeFourths`,
`lastThreeFourths`, `firstFifth`, `secondFifth`, `thirdFifth`, `fourthFifth`,
`lastFifth`, `firstSixth`, `lastSixth`

**Corners & grid cells** (31)
`topLeft`, `topRight`, `bottomLeft`, `bottomRight`, `topLeftThird`,
`topRightThird`, `bottomLeftThird`, `bottomRightThird`, `topLeftSixth`,
`topCenterSixth`, `topRightSixth`, `bottomLeftSixth`, `bottomCenterSixth`,
`bottomRightSixth`, `topLeftNinth`, `topCenterNinth`, `topRightNinth`,
`middleLeftNinth`, `middleCenterNinth`, `middleRightNinth`, `bottomLeftNinth`,
`bottomCenterNinth`, `bottomRightNinth`, `topLeftEighth`, `topCenterLeftEighth`,
`topCenterRightEighth`, `topRightEighth`, `bottomLeftEighth`,
`bottomCenterLeftEighth`, `bottomCenterRightEighth`, `bottomRightEighth`

**Maximize & center** (6)
`maximize`, `almostMaximize`, `maximizeWidth`, `center`, `centerProminently`,
`upperCenter`

**Fill — remaining corners** (4)
`fillTopLeft`, `fillTopRight`, `fillBottomLeft`, `fillBottomRight`

**Resize** (10)
`largerWidth`, `smallerWidth`, `doubleHeightUp`, `doubleHeightDown`,
`doubleWidthLeft`, `doubleWidthRight`, `halveHeightUp`, `halveHeightDown`,
`halveWidthLeft`, `halveWidthRight`

**Move & nudge** (8)
`moveUp`, `moveDown`, `moveLeft`, `moveRight`, `nudgeUp`, `nudgeDown`,
`nudgeLeft`, `nudgeRight`

**Displays & spaces** (5)
`nextDisplay`, `nextDisplayRatio`, `prevDisplayRatio`, `nextSpace`, `prevSpace`

**Multi-window / app-scoped** (9)
`appLeftHalf`, `appRightHalf`, `appNextDisplay`, `appPrevDisplay`, `cascadeApp`,
`cascadeAll`, `reverseAll`, `tile2x2`, `tile2x3`

**Stash** (10)
`stashUp`, `stashDown`, `stashLeft`, `stashRight`, `stashAll`,
`stashAllButFront`, `toggleStashed`, `cycleStashed`, `unstash`, `unstashAll`

**Todo mode** (2)
`leftTodo`, `rightTodo`

**Snap & pin** (8)
`snapTopLeft`, `snapTopRight`, `snapBottomLeft`, `snapBottomRight`, `pin`,
`reflowPin`, `revealDesktopEdge`, `specified`

## Plist format

Each action is a dict keyed by its action name. Empty dict = unbound. A bound one
looks like:

```xml
<key>fillLeft</key>
<dict>
    <key>keyCode</key>      <integer>4</integer>       <!-- kVK_ANSI_H -->
    <key>modifierFlags</key><integer>1310720</integer> <!-- ⌃⌘ -->
    <key>type</key>         <integer>0</integer>
</dict>
```

`keyCode` is a Carbon virtual keycode (`kVK_*` in `HIToolbox/Events.h`).
`modifierFlags` is an `NSEvent.ModifierFlags` bitmask:

| Bit | Value | Modifier |
| --- | --- | --- |
| 16 | 65536 | Caps Lock |
| 17 | 131072 | ⇧ Shift |
| 18 | 262144 | ⌃ Control |
| 19 | 524288 | ⌥ Option |
| 20 | 1048576 | ⌘ Command |
| 23 | 8388608 | fn |

So `1310720` = 1048576 + 262144 = ⌃⌘, and `1179648` = 1048576 + 131072 = ⇧⌘.
