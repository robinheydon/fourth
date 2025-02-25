///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const math = @import("math.zig");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Color = enum(u32) {
    // stored internally as ABGR

    // License: https://creativecommons.org/publicdomain/zero/1.0/
    @"acid green" = raw(0x8ffe09),
    @"algae green" = raw(0x21c36f),
    @"almost black" = raw(0x070d0d),
    @"apple green" = raw(0x76cd26),
    @"aqua blue" = raw(0x02d8e9),
    @"aqua green" = raw(0x12e193),
    @"aqua marine" = raw(0x2ee8bb),
    @"army green" = raw(0x4b5d16),
    @"avocado green" = raw(0x87a922),
    @"baby blue" = raw(0xa2cffe),
    @"baby green" = raw(0x8cff9e),
    @"baby pink" = raw(0xffb7ce),
    @"baby poo" = raw(0xab9004),
    @"baby poop green" = raw(0x8f9805),
    @"baby poop" = raw(0x937c00),
    @"baby puke green" = raw(0xb6c406),
    @"baby purple" = raw(0xca9bf7),
    @"baby shit brown" = raw(0xad900d),
    @"baby shit green" = raw(0x889717),
    @"banana yellow" = raw(0xfafe4b),
    @"barbie pink" = raw(0xfe46a5),
    @"barf green" = raw(0x94ac02),
    @"barney purple" = raw(0xa00498),
    @"battleship grey" = raw(0x6b7c85),
    @"blood orange" = raw(0xfe4b03),
    @"blood red" = raw(0x980002),
    @"blue blue" = raw(0x2242c7),
    @"blue green" = raw(0x137e6d),
    @"blue grey" = raw(0x607c8e),
    @"blue purple" = raw(0x5729ce),
    @"blue violet" = raw(0x5d06e9),
    @"blue with a hint of purple" = raw(0x533cc6),
    @"blue/green" = raw(0x0f9b8e),
    @"blue/grey" = raw(0x758da3),
    @"blue/purple" = raw(0x5a06ef),
    @"bluey green" = raw(0x2bb179),
    @"bluey grey" = raw(0x89a0b0),
    @"bluey purple" = raw(0x6241c7),
    @"bluish green" = raw(0x10a674),
    @"bluish grey" = raw(0x748b97),
    @"bluish purple" = raw(0x703be7),
    @"blush pink" = raw(0xfe828c),
    @"booger green" = raw(0x96b403),
    @"boring green" = raw(0x63b365),
    @"bottle green" = raw(0x044a05),
    @"brick orange" = raw(0xc14a09),
    @"brick red" = raw(0x8f1402),
    @"bright aqua" = raw(0x0bf9ea),
    @"bright blue" = raw(0x0165fc),
    @"bright cyan" = raw(0x41fdfe),
    @"bright green" = raw(0x01ff07),
    @"bright lavender" = raw(0xc760ff),
    @"bright light blue" = raw(0x26f7fd),
    @"bright light green" = raw(0x2dfe54),
    @"bright lilac" = raw(0xc95efb),
    @"bright lime green" = raw(0x65fe08),
    @"bright lime" = raw(0x87fd05),
    @"bright magenta" = raw(0xff08e8),
    @"bright olive" = raw(0x9cbb04),
    @"bright orange" = raw(0xff5b00),
    @"bright pink" = raw(0xfe01b1),
    @"bright purple" = raw(0xbe03fd),
    @"bright red" = raw(0xff000d),
    @"bright sea green" = raw(0x05ffa6),
    @"bright sky blue" = raw(0x02ccfe),
    @"bright teal" = raw(0x01f9c6),
    @"bright turquoise" = raw(0x0ffef9),
    @"bright violet" = raw(0xad0afd),
    @"bright yellow green" = raw(0x9dff00),
    @"bright yellow" = raw(0xfffd01),
    @"british racing green" = raw(0x05480d),
    @"brown green" = raw(0x706c11),
    @"brown grey" = raw(0x8d8468),
    @"brown orange" = raw(0xb96902),
    @"brown red" = raw(0x922b05),
    @"brown yellow" = raw(0xb29705),
    @"brownish green" = raw(0x6a6e09),
    @"brownish grey" = raw(0x86775f),
    @"brownish orange" = raw(0xcb7723),
    @"brownish pink" = raw(0xc27e79),
    @"brownish purple" = raw(0x76424e),
    @"brownish red" = raw(0x9e3623),
    @"brownish yellow" = raw(0xc9b003),
    @"browny green" = raw(0x6f6c0a),
    @"browny orange" = raw(0xca6b02),
    @"bubble gum pink" = raw(0xff69af),
    @"bubblegum pink" = raw(0xfe83cc),
    @"burnt orange" = raw(0xc04e01),
    @"burnt red" = raw(0x9f2305),
    @"burnt siena" = raw(0xb75203),
    @"burnt sienna" = raw(0xb04e0f),
    @"burnt umber" = raw(0xa0450e),
    @"burnt yellow" = raw(0xd5ab09),
    @"butter yellow" = raw(0xfffd74),
    @"cadet blue" = raw(0x4e7496),
    @"camo green" = raw(0x526525),
    @"camouflage green" = raw(0x4b6113),
    @"canary yellow" = raw(0xfffe40),
    @"candy pink" = raw(0xff63e9),
    @"carnation pink" = raw(0xff7fa7),
    @"carolina blue" = raw(0x8ab8fe),
    @"cerulean blue" = raw(0x056eee),
    @"charcoal grey" = raw(0x3c4142),
    @"cherry red" = raw(0xf7022a),
    @"chocolate brown" = raw(0x411900),
    @"clay brown" = raw(0xb2713d),
    @"clear blue" = raw(0x247afd),
    @"cloudy blue" = raw(0xacc2d9),
    @"cobalt blue" = raw(0x030aa7),
    @"cool blue" = raw(0x4984b8),
    @"cool green" = raw(0x33b864),
    @"cool grey" = raw(0x95a3a6),
    @"coral pink" = raw(0xff6163),
    @"cornflower blue" = raw(0x5170d7),
    @"dark aqua" = raw(0x05696b),
    @"dark aquamarine" = raw(0x017371),
    @"dark beige" = raw(0xac9362),
    @"dark blue green" = raw(0x005249),
    @"dark blue grey" = raw(0x1f3b4d),
    @"dark blue" = raw(0x00035b),
    @"dark brown" = raw(0x341c02),
    @"dark coral" = raw(0xcf524e),
    @"dark cream" = raw(0xfff39a),
    @"dark cyan" = raw(0x0a888a),
    @"dark forest green" = raw(0x002d04),
    @"dark fuchsia" = raw(0x9d0759),
    @"dark gold" = raw(0xb59410),
    @"dark grass green" = raw(0x388004),
    @"dark green blue" = raw(0x1f6357),
    @"dark green" = raw(0x033500),
    @"dark grey blue" = raw(0x29465b),
    @"dark grey" = raw(0x363737),
    @"dark hot pink" = raw(0xd90166),
    @"dark indigo" = raw(0x1f0954),
    @"dark khaki" = raw(0x9b8f55),
    @"dark lavender" = raw(0x856798),
    @"dark lilac" = raw(0x9c6da5),
    @"dark lime green" = raw(0x7ebd01),
    @"dark lime" = raw(0x84b701),
    @"dark magenta" = raw(0x960056),
    @"dark maroon" = raw(0x3c0008),
    @"dark mauve" = raw(0x874c62),
    @"dark mint green" = raw(0x20c073),
    @"dark mint" = raw(0x48c072),
    @"dark mustard" = raw(0xa88905),
    @"dark navy blue" = raw(0x00022e),
    @"dark navy" = raw(0x000435),
    @"dark olive green" = raw(0x3c4d03),
    @"dark olive" = raw(0x373e02),
    @"dark orange" = raw(0xc65102),
    @"dark pastel green" = raw(0x56ae57),
    @"dark peach" = raw(0xde7e5d),
    @"dark periwinkle" = raw(0x665fd1),
    @"dark pink" = raw(0xcb416b),
    @"dark plum" = raw(0x3f012c),
    @"dark purple" = raw(0x35063e),
    @"dark red" = raw(0x840000),
    @"dark rose" = raw(0xb5485d),
    @"dark royal blue" = raw(0x02066f),
    @"dark sage" = raw(0x598556),
    @"dark salmon" = raw(0xc85a53),
    @"dark sand" = raw(0xa88f59),
    @"dark sea green" = raw(0x11875d),
    @"dark seafoam green" = raw(0x3eaf76),
    @"dark seafoam" = raw(0x1fb57a),
    @"dark sky blue" = raw(0x448ee4),
    @"dark slate blue" = raw(0x214761),
    @"dark tan" = raw(0xaf884a),
    @"dark taupe" = raw(0x7f684e),
    @"dark teal" = raw(0x014d4e),
    @"dark turquoise" = raw(0x045c5a),
    @"dark violet" = raw(0x34013f),
    @"dark yellow green" = raw(0x728f02),
    @"dark yellow" = raw(0xd5b60a),
    @"darkish blue" = raw(0x014182),
    @"darkish green" = raw(0x287c37),
    @"darkish pink" = raw(0xda467d),
    @"darkish purple" = raw(0x751973),
    @"darkish red" = raw(0xa90308),
    @"deep aqua" = raw(0x08787f),
    @"deep blue" = raw(0x040273),
    @"deep brown" = raw(0x410200),
    @"deep green" = raw(0x02590f),
    @"deep lavender" = raw(0x8d5eb7),
    @"deep lilac" = raw(0x966ebd),
    @"deep magenta" = raw(0xa0025c),
    @"deep orange" = raw(0xdc4d01),
    @"deep pink" = raw(0xcb0162),
    @"deep purple" = raw(0x36013f),
    @"deep red" = raw(0x9a0200),
    @"deep rose" = raw(0xc74767),
    @"deep sea blue" = raw(0x015482),
    @"deep sky blue" = raw(0x0d75f8),
    @"deep teal" = raw(0x00555a),
    @"deep turquoise" = raw(0x017374),
    @"deep violet" = raw(0x490648),
    @"denim blue" = raw(0x3b5b92),
    @"dirt brown" = raw(0x836539),
    @"dirty blue" = raw(0x3f829d),
    @"dirty green" = raw(0x667e2c),
    @"dirty orange" = raw(0xc87606),
    @"dirty pink" = raw(0xca7b80),
    @"dirty purple" = raw(0x734a65),
    @"dirty yellow" = raw(0xcdc50a),
    @"dodger blue" = raw(0x3e82fc),
    @"drab green" = raw(0x749551),
    @"dried blood" = raw(0x4b0101),
    @"duck egg blue" = raw(0xc3fbf4),
    @"dull blue" = raw(0x49759c),
    @"dull brown" = raw(0x876e4b),
    @"dull green" = raw(0x74a662),
    @"dull orange" = raw(0xd8863b),
    @"dull pink" = raw(0xd5869d),
    @"dull purple" = raw(0x84597e),
    @"dull red" = raw(0xbb3f3f),
    @"dull teal" = raw(0x5f9e8f),
    @"dull yellow" = raw(0xeedc5b),
    @"dusk blue" = raw(0x26538d),
    @"dusky blue" = raw(0x475f94),
    @"dusky pink" = raw(0xcc7a8b),
    @"dusky purple" = raw(0x895b7b),
    @"dusky rose" = raw(0xba6873),
    @"dusty blue" = raw(0x5a86ad),
    @"dusty green" = raw(0x76a973),
    @"dusty lavender" = raw(0xac86a8),
    @"dusty orange" = raw(0xf0833a),
    @"dusty pink" = raw(0xd58a94),
    @"dusty purple" = raw(0x825f87),
    @"dusty red" = raw(0xb9484e),
    @"dusty rose" = raw(0xc0737a),
    @"dusty teal" = raw(0x4c9085),
    @"easter green" = raw(0x8cfd7e),
    @"easter purple" = raw(0xc071fe),
    @"egg shell" = raw(0xfffcc4),
    @"eggplant purple" = raw(0x430541),
    @"eggshell blue" = raw(0xc4fff7),
    @"electric blue" = raw(0x0652ff),
    @"electric green" = raw(0x21fc0d),
    @"electric lime" = raw(0xa8ff04),
    @"electric pink" = raw(0xff0490),
    @"electric purple" = raw(0xaa23ff),
    @"emerald green" = raw(0x028f1e),
    @"faded blue" = raw(0x658cbb),
    @"faded green" = raw(0x7bb274),
    @"faded orange" = raw(0xf0944d),
    @"faded pink" = raw(0xde9dac),
    @"faded purple" = raw(0x916e99),
    @"faded red" = raw(0xd3494e),
    @"faded yellow" = raw(0xfeff7f),
    @"fern green" = raw(0x548d44),
    @"fire engine red" = raw(0xfe0002),
    @"flat blue" = raw(0x3c73a8),
    @"flat green" = raw(0x699d4c),
    @"fluorescent green" = raw(0x08ff08),
    @"fluro green" = raw(0x0aff02),
    @"foam green" = raw(0x90fda9),
    @"forest green" = raw(0x06470c),
    @"forrest green" = raw(0x154406),
    @"french blue" = raw(0x436bad),
    @"fresh green" = raw(0x69d84f),
    @"frog green" = raw(0x58bc08),
    @"golden brown" = raw(0xb27a01),
    @"golden rod" = raw(0xf9bc08),
    @"golden yellow" = raw(0xfec615),
    @"grape purple" = raw(0x5d1451),
    @"grass green" = raw(0x3f9b0b),
    @"grassy green" = raw(0x419c03),
    @"green apple" = raw(0x5edc1f),
    @"green blue" = raw(0x06b48b),
    @"green brown" = raw(0x544e03),
    @"green grey" = raw(0x77926f),
    @"green teal" = raw(0x0cb577),
    @"green yellow" = raw(0xc9ff27),
    @"green/blue" = raw(0x01c08d),
    @"green/yellow" = raw(0xb5ce08),
    @"greenish beige" = raw(0xc9d179),
    @"greenish blue" = raw(0x0b8b87),
    @"greenish brown" = raw(0x696112),
    @"greenish cyan" = raw(0x2afeb7),
    @"greenish grey" = raw(0x96ae8d),
    @"greenish tan" = raw(0xbccb7a),
    @"greenish teal" = raw(0x32bf84),
    @"greenish turquoise" = raw(0x00fbb0),
    @"greenish yellow" = raw(0xcdfd02),
    @"greeny blue" = raw(0x42b395),
    @"greeny brown" = raw(0x696006),
    @"greeny grey" = raw(0x7ea07a),
    @"greeny yellow" = raw(0xc6f808),
    @"grey blue" = raw(0x6b8ba4),
    @"grey brown" = raw(0x7f7053),
    @"grey green" = raw(0x789b73),
    @"grey pink" = raw(0xc3909b),
    @"grey purple" = raw(0x826d8c),
    @"grey teal" = raw(0x5e9b8a),
    @"grey/blue" = raw(0x647d8e),
    @"grey/green" = raw(0x86a17d),
    @"greyish blue" = raw(0x5e819d),
    @"greyish brown" = raw(0x7a6a4f),
    @"greyish green" = raw(0x82a67d),
    @"greyish pink" = raw(0xc88d94),
    @"greyish purple" = raw(0x887191),
    @"greyish teal" = raw(0x719f91),
    @"gross green" = raw(0xa0bf16),
    @"highlighter green" = raw(0x1bfc06),
    @"hospital green" = raw(0x9be5aa),
    @"hot green" = raw(0x25ff29),
    @"hot magenta" = raw(0xf504c9),
    @"hot pink" = raw(0xff028d),
    @"hot purple" = raw(0xcb00f5),
    @"hunter green" = raw(0x0b4008),
    @"ice blue" = raw(0xd7fffe),
    @"icky green" = raw(0x8fae22),
    @"indian red" = raw(0x850e04),
    @"indigo blue" = raw(0x3a18b1),
    @"irish green" = raw(0x019529),
    @"jade green" = raw(0x2baf6a),
    @"jungle green" = raw(0x048243),
    @"kelley green" = raw(0x009337),
    @"kelly green" = raw(0x02ab2e),
    @"kermit green" = raw(0x5cb200),
    @"key lime" = raw(0xaeff6e),
    @"khaki green" = raw(0x728639),
    @"kiwi green" = raw(0x8ee53f),
    @"lavender blue" = raw(0x8b88f8),
    @"lavender pink" = raw(0xdd85d7),
    @"lawn green" = raw(0x4da409),
    @"leaf green" = raw(0x5ca904),
    @"leafy green" = raw(0x51b73b),
    @"lemon green" = raw(0xadf802),
    @"lemon lime" = raw(0xbffe28),
    @"lemon yellow" = raw(0xfdff38),
    @"light aqua" = raw(0x8cffdb),
    @"light aquamarine" = raw(0x7bfdc7),
    @"light beige" = raw(0xfffeb6),
    @"light blue green" = raw(0x7efbb3),
    @"light blue grey" = raw(0xb7c9e2),
    @"light blue" = raw(0x95d0fc),
    @"light bluish green" = raw(0x76fda8),
    @"light bright green" = raw(0x53fe5c),
    @"light brown" = raw(0xad8150),
    @"light burgundy" = raw(0xa8415b),
    @"light cyan" = raw(0xacfffc),
    @"light eggplant" = raw(0x894585),
    @"light forest green" = raw(0x4f9153),
    @"light gold" = raw(0xfddc5c),
    @"light grass green" = raw(0x9af764),
    @"light green blue" = raw(0x56fca2),
    @"light green" = raw(0x96f97b),
    @"light greenish blue" = raw(0x63f7b4),
    @"light grey blue" = raw(0x9dbcd4),
    @"light grey green" = raw(0xb7e1a1),
    @"light grey" = raw(0xd8dcd6),
    @"light indigo" = raw(0x6d5acf),
    @"light khaki" = raw(0xe6f2a2),
    @"light lavendar" = raw(0xefc0fe),
    @"light lavender" = raw(0xdfc5fe),
    @"light light blue" = raw(0xcafffb),
    @"light light green" = raw(0xc8ffb0),
    @"light lilac" = raw(0xedc8ff),
    @"light lime green" = raw(0xb9ff66),
    @"light lime" = raw(0xaefd6c),
    @"light magenta" = raw(0xfa5ff7),
    @"light maroon" = raw(0xa24857),
    @"light mauve" = raw(0xc292a1),
    @"light mint green" = raw(0xa6fbb2),
    @"light mint" = raw(0xb6ffbb),
    @"light moss green" = raw(0xa6c875),
    @"light mustard" = raw(0xf7d560),
    @"light navy blue" = raw(0x2e5a88),
    @"light navy" = raw(0x155084),
    @"light neon green" = raw(0x4efd54),
    @"light olive green" = raw(0xa4be5c),
    @"light olive" = raw(0xacbf69),
    @"light orange" = raw(0xfdaa48),
    @"light pastel green" = raw(0xb2fba5),
    @"light pea green" = raw(0xc4fe82),
    @"light peach" = raw(0xffd8b1),
    @"light periwinkle" = raw(0xc1c6fc),
    @"light pink" = raw(0xffd1df),
    @"light plum" = raw(0x9d5783),
    @"light purple" = raw(0xbf77f6),
    @"light red" = raw(0xff474c),
    @"light rose" = raw(0xffc5cb),
    @"light royal blue" = raw(0x3a2efe),
    @"light sage" = raw(0xbcecac),
    @"light salmon" = raw(0xfea993),
    @"light sea green" = raw(0x98f6b0),
    @"light seafoam green" = raw(0xa7ffb5),
    @"light seafoam" = raw(0xa0febf),
    @"light sky blue" = raw(0xc6fcff),
    @"light tan" = raw(0xfbeeac),
    @"light teal" = raw(0x90e4c1),
    @"light turquoise" = raw(0x7ef4cc),
    @"light urple" = raw(0xb36ff6),
    @"light violet" = raw(0xd6b4fc),
    @"light yellow green" = raw(0xccfd7f),
    @"light yellow" = raw(0xfffe7a),
    @"light yellowish green" = raw(0xc2ff89),
    @"lighter green" = raw(0x75fd63),
    @"lighter purple" = raw(0xa55af4),
    @"lightish blue" = raw(0x3d7afd),
    @"lightish green" = raw(0x61e160),
    @"lightish purple" = raw(0xa552e6),
    @"lightish red" = raw(0xfe2f4a),
    @"lime green" = raw(0x89fe05),
    @"lime yellow" = raw(0xd0fe1d),
    @"lipstick red" = raw(0xc0022f),
    @"macaroni and cheese" = raw(0xefb435),
    @"marine blue" = raw(0x01386a),
    @"medium blue" = raw(0x2c6fbb),
    @"medium brown" = raw(0x7f5112),
    @"medium green" = raw(0x39ad48),
    @"medium grey" = raw(0x7d7f7c),
    @"medium pink" = raw(0xf36196),
    @"medium purple" = raw(0x9e43a2),
    @"metallic blue" = raw(0x4f738e),
    @"mid blue" = raw(0x276ab3),
    @"mid green" = raw(0x50a747),
    @"midnight blue" = raw(0x020035),
    @"midnight purple" = raw(0x280137),
    @"military green" = raw(0x667c3e),
    @"milk chocolate" = raw(0x7f4e1e),
    @"mint green" = raw(0x8fff9f),
    @"minty green" = raw(0x0bf77d),
    @"moss green" = raw(0x658b38),
    @"mossy green" = raw(0x638b27),
    @"mud brown" = raw(0x60460f),
    @"mud green" = raw(0x606602),
    @"muddy brown" = raw(0x886806),
    @"muddy green" = raw(0x657432),
    @"muddy yellow" = raw(0xbfac05),
    @"murky green" = raw(0x6c7a0e),
    @"mustard brown" = raw(0xac7e04),
    @"mustard green" = raw(0xa8b504),
    @"mustard yellow" = raw(0xd2bd0a),
    @"muted blue" = raw(0x3b719f),
    @"muted green" = raw(0x5fa052),
    @"muted pink" = raw(0xd1768f),
    @"muted purple" = raw(0x805b87),
    @"nasty green" = raw(0x70b23f),
    @"navy blue" = raw(0x001146),
    @"navy green" = raw(0x35530a),
    @"neon blue" = raw(0x04d9ff),
    @"neon green" = raw(0x0cff0c),
    @"neon pink" = raw(0xfe019a),
    @"neon purple" = raw(0xbc13fe),
    @"neon red" = raw(0xff073a),
    @"neon yellow" = raw(0xcfff04),
    @"nice blue" = raw(0x107ab0),
    @"night blue" = raw(0x040348),
    @"ocean blue" = raw(0x03719c),
    @"ocean green" = raw(0x3d9973),
    @"off blue" = raw(0x5684ae),
    @"off green" = raw(0x6ba353),
    @"off white" = raw(0xffffe4),
    @"off yellow" = raw(0xf1f33f),
    @"old pink" = raw(0xc77986),
    @"old rose" = raw(0xc87f89),
    @"olive brown" = raw(0x645403),
    @"olive drab" = raw(0x6f7632),
    @"olive green" = raw(0x677a04),
    @"olive yellow" = raw(0xc2b709),
    @"orange brown" = raw(0xbe6400),
    @"orange pink" = raw(0xff6f52),
    @"orange red" = raw(0xfd411e),
    @"orange yellow" = raw(0xffad01),
    @"orangey brown" = raw(0xb16002),
    @"orangey red" = raw(0xfa4224),
    @"orangey yellow" = raw(0xfdb915),
    @"orangish brown" = raw(0xb25f03),
    @"orangish red" = raw(0xf43605),
    @"pale aqua" = raw(0xb8ffeb),
    @"pale blue" = raw(0xd0fefe),
    @"pale brown" = raw(0xb1916e),
    @"pale cyan" = raw(0xb7fffa),
    @"pale gold" = raw(0xfdde6c),
    @"pale green" = raw(0xc7fdb5),
    @"pale grey" = raw(0xfdfdfe),
    @"pale lavender" = raw(0xeecffe),
    @"pale light green" = raw(0xb1fc99),
    @"pale lilac" = raw(0xe4cbff),
    @"pale lime green" = raw(0xb1ff65),
    @"pale lime" = raw(0xbefd73),
    @"pale magenta" = raw(0xd767ad),
    @"pale mauve" = raw(0xfed0fc),
    @"pale olive green" = raw(0xb1d27b),
    @"pale olive" = raw(0xb9cc81),
    @"pale orange" = raw(0xffa756),
    @"pale peach" = raw(0xffe5ad),
    @"pale pink" = raw(0xffcfdc),
    @"pale purple" = raw(0xb790d4),
    @"pale red" = raw(0xd9544d),
    @"pale rose" = raw(0xfdc1c5),
    @"pale salmon" = raw(0xffb19a),
    @"pale sky blue" = raw(0xbdf6fe),
    @"pale teal" = raw(0x82cbb2),
    @"pale turquoise" = raw(0xa5fbd5),
    @"pale violet" = raw(0xceaefa),
    @"pale yellow" = raw(0xffff84),
    @"pastel blue" = raw(0xa2bffe),
    @"pastel green" = raw(0xb0ff9d),
    @"pastel orange" = raw(0xff964f),
    @"pastel pink" = raw(0xffbacd),
    @"pastel purple" = raw(0xcaa0ff),
    @"pastel red" = raw(0xdb5856),
    @"pastel yellow" = raw(0xfffe71),
    @"pea green" = raw(0x8eab12),
    @"pea soup green" = raw(0x94a617),
    @"pea soup" = raw(0x929901),
    @"peachy pink" = raw(0xff9a8a),
    @"peacock blue" = raw(0x016795),
    @"periwinkle blue" = raw(0x8f99fb),
    @"pig pink" = raw(0xe78ea5),
    @"pine green" = raw(0x0a481e),
    @"pink purple" = raw(0xdb4bda),
    @"pink red" = raw(0xf5054f),
    @"pink/purple" = raw(0xef1de7),
    @"pinkish brown" = raw(0xb17261),
    @"pinkish grey" = raw(0xc8aca9),
    @"pinkish orange" = raw(0xff724c),
    @"pinkish purple" = raw(0xd648d7),
    @"pinkish red" = raw(0xf10c45),
    @"pinkish tan" = raw(0xd99b82),
    @"pinky purple" = raw(0xc94cbe),
    @"pinky red" = raw(0xfc2647),
    @"piss yellow" = raw(0xddd618),
    @"plum purple" = raw(0x4e0550),
    @"poison green" = raw(0x40fd14),
    @"poo brown" = raw(0x885f01),
    @"poop brown" = raw(0x7a5901),
    @"poop green" = raw(0x6f7c00),
    @"powder blue" = raw(0xb1d1fc),
    @"powder pink" = raw(0xffb2d0),
    @"primary blue" = raw(0x0804f9),
    @"prussian blue" = raw(0x004577),
    @"puke brown" = raw(0x947706),
    @"puke green" = raw(0x9aae07),
    @"puke yellow" = raw(0xc2be0e),
    @"pumpkin orange" = raw(0xfb7d07),
    @"pure blue" = raw(0x0203e2),
    @"purple blue" = raw(0x632de9),
    @"purple brown" = raw(0x673a3f),
    @"purple grey" = raw(0x866f85),
    @"purple pink" = raw(0xe03fd8),
    @"purple red" = raw(0x990147),
    @"purple/blue" = raw(0x5d21d0),
    @"purple/pink" = raw(0xd725de),
    @"purpleish blue" = raw(0x6140ef),
    @"purpleish pink" = raw(0xdf4ec8),
    @"purpley blue" = raw(0x5f34e7),
    @"purpley grey" = raw(0x947e94),
    @"purpley pink" = raw(0xc83cb9),
    @"purplish blue" = raw(0x601ef9),
    @"purplish brown" = raw(0x6b4247),
    @"purplish grey" = raw(0x7a687f),
    @"purplish pink" = raw(0xce5dae),
    @"purplish red" = raw(0xb0054b),
    @"purply blue" = raw(0x661aee),
    @"purply pink" = raw(0xf075e6),
    @"racing green" = raw(0x014600),
    @"radioactive green" = raw(0x2cfa1f),
    @"raw sienna" = raw(0x9a6200),
    @"raw umber" = raw(0xa75e09),
    @"really light blue" = raw(0xd4ffff),
    @"red brown" = raw(0x8b2e16),
    @"red orange" = raw(0xfd3c06),
    @"red pink" = raw(0xfa2a55),
    @"red purple" = raw(0x820747),
    @"red violet" = raw(0x9e0168),
    @"red wine" = raw(0x8c0034),
    @"reddish brown" = raw(0x7f2b0a),
    @"reddish grey" = raw(0x997570),
    @"reddish orange" = raw(0xf8481c),
    @"reddish pink" = raw(0xfe2c54),
    @"reddish purple" = raw(0x910951),
    @"reddy brown" = raw(0x6e1005),
    @"rich blue" = raw(0x021bf9),
    @"rich purple" = raw(0x720058),
    @"robin egg blue" = raw(0x8af1fe),
    @"robin's egg blue" = raw(0x98eff9),
    @"robin's egg" = raw(0x6dedfd),
    @"rose pink" = raw(0xf7879a),
    @"rose red" = raw(0xbe013c),
    @"rosy pink" = raw(0xf6688e),
    @"royal blue" = raw(0x0504aa),
    @"royal purple" = raw(0x4b006e),
    @"rust brown" = raw(0x8b3103),
    @"rust orange" = raw(0xc45508),
    @"rust red" = raw(0xaa2704),
    @"rusty orange" = raw(0xcd5909),
    @"rusty red" = raw(0xaf2f0d),
    @"sage green" = raw(0x88b378),
    @"salmon pink" = raw(0xfe7b7c),
    @"sand brown" = raw(0xcba560),
    @"sand yellow" = raw(0xfce166),
    @"sandy brown" = raw(0xc4a661),
    @"sandy yellow" = raw(0xfdee73),
    @"sap green" = raw(0x5c8b15),
    @"sea blue" = raw(0x047495),
    @"sea green" = raw(0x53fca1),
    @"seafoam blue" = raw(0x78d1b6),
    @"seafoam green" = raw(0x7af9ab),
    @"seaweed green" = raw(0x35ad6b),
    @"shamrock green" = raw(0x02c14d),
    @"shit brown" = raw(0x7b5804),
    @"shit green" = raw(0x758000),
    @"shocking pink" = raw(0xfe02a2),
    @"sick green" = raw(0x9db92c),
    @"sickly green" = raw(0x94b21c),
    @"sickly yellow" = raw(0xd0e429),
    @"sky blue" = raw(0x75bbfd),
    @"slate blue" = raw(0x5b7c99),
    @"slate green" = raw(0x658d6d),
    @"slate grey" = raw(0x59656d),
    @"slime green" = raw(0x99cc04),
    @"snot green" = raw(0x9dc100),
    @"soft blue" = raw(0x6488ea),
    @"soft green" = raw(0x6fc276),
    @"soft pink" = raw(0xfdb0c0),
    @"soft purple" = raw(0xa66fb5),
    @"spring green" = raw(0xa9f971),
    @"steel blue" = raw(0x5a7d9a),
    @"steel grey" = raw(0x6f828a),
    @"stormy blue" = raw(0x507b9c),
    @"strong blue" = raw(0x0c06f7),
    @"strong pink" = raw(0xff0789),
    @"sun yellow" = raw(0xffdf22),
    @"sunflower yellow" = raw(0xffda03),
    @"sunny yellow" = raw(0xfff917),
    @"sunshine yellow" = raw(0xfffd37),
    @"swamp green" = raw(0x748500),
    @"tan brown" = raw(0xab7e4c),
    @"tan green" = raw(0xa9be70),
    @"tea green" = raw(0xbdf8a3),
    @"teal blue" = raw(0x01889f),
    @"teal green" = raw(0x25a36f),
    @"tealish green" = raw(0x0cdc73),
    @"terra cotta" = raw(0xc9643b),
    @"tiffany blue" = raw(0x7bf2da),
    @"tomato red" = raw(0xec2d01),
    @"toxic green" = raw(0x61de2a),
    @"tree green" = raw(0x2a7e19),
    @"true blue" = raw(0x010fcc),
    @"true green" = raw(0x089404),
    @"turquoise blue" = raw(0x06b1c4),
    @"turquoise green" = raw(0x04f489),
    @"turtle green" = raw(0x75b84f),
    @"twilight blue" = raw(0x0a437a),
    @"ugly blue" = raw(0x31668a),
    @"ugly brown" = raw(0x7d7103),
    @"ugly green" = raw(0x7a9703),
    @"ugly pink" = raw(0xcd7584),
    @"ugly purple" = raw(0xa442a0),
    @"ugly yellow" = raw(0xd0c101),
    @"ultramarine blue" = raw(0x1805db),
    @"very dark blue" = raw(0x000133),
    @"very dark brown" = raw(0x1d0200),
    @"very dark green" = raw(0x062e03),
    @"very dark purple" = raw(0x2a0134),
    @"very light blue" = raw(0xd5ffff),
    @"very light brown" = raw(0xd3b683),
    @"very light green" = raw(0xd1ffbd),
    @"very light pink" = raw(0xfff4f2),
    @"very light purple" = raw(0xf6cefc),
    @"very pale blue" = raw(0xd6fffe),
    @"very pale green" = raw(0xcffdbc),
    @"vibrant blue" = raw(0x0339f8),
    @"vibrant green" = raw(0x0add08),
    @"vibrant purple" = raw(0xad03de),
    @"violet blue" = raw(0x510ac9),
    @"violet pink" = raw(0xfb5ffc),
    @"violet red" = raw(0xa50055),
    @"vivid blue" = raw(0x152eff),
    @"vivid green" = raw(0x2fef10),
    @"vivid purple" = raw(0x9900fa),
    @"vomit green" = raw(0x89a203),
    @"vomit yellow" = raw(0xc7c10c),
    @"warm blue" = raw(0x4b57db),
    @"warm brown" = raw(0x964e02),
    @"warm grey" = raw(0x978a84),
    @"warm pink" = raw(0xfb5581),
    @"warm purple" = raw(0x952e8f),
    @"washed out green" = raw(0xbcf5a6),
    @"water blue" = raw(0x0e87cc),
    @"weird green" = raw(0x3ae57f),
    @"windows blue" = raw(0x3778bf),
    @"wine red" = raw(0x7b0323),
    @"yellow brown" = raw(0xb79400),
    @"yellow green" = raw(0xc0fb2d),
    @"yellow ochre" = raw(0xcb9d06),
    @"yellow orange" = raw(0xfcb001),
    @"yellow tan" = raw(0xffe36e),
    @"yellow/green" = raw(0xc8fd3d),
    @"yellowish brown" = raw(0x9b7a01),
    @"yellowish green" = raw(0xb0dd16),
    @"yellowish orange" = raw(0xffab0f),
    @"yellowish tan" = raw(0xfcfc81),
    @"yellowy brown" = raw(0xae8b0c),
    @"yellowy green" = raw(0xbff128),
    adobe = raw(0xbd6c48),
    algae = raw(0x54ac68),
    amber = raw(0xfeb308),
    amethyst = raw(0x9b5fc0),
    apple = raw(0x6ecb3c),
    apricot = raw(0xffb16d),
    aqua = raw(0x13eac9),
    aquamarine = raw(0x04d8b2),
    asparagus = raw(0x77ab56),
    aubergine = raw(0x3d0734),
    auburn = raw(0x9a3001),
    avocado = raw(0x90b134),
    azul = raw(0x1d5dec),
    azure = raw(0x069af3),
    banana = raw(0xffff7e),
    barney = raw(0xac1db8),
    beige = raw(0xe6daa6),
    berry = raw(0x990f4b),
    bile = raw(0xb5c306),
    black = raw(0x000000),
    bland = raw(0xafa88b),
    blood = raw(0x770001),
    blue = raw(0x0343df),
    blueberry = raw(0x464196),
    bluegreen = raw(0x017a79),
    bluegrey = raw(0x85a3b2),
    bluish = raw(0x2976bb),
    blurple = raw(0x5539cc),
    blush = raw(0xf29e8e),
    booger = raw(0x9bb53c),
    bordeaux = raw(0x7b002c),
    brick = raw(0xa03623),
    bronze = raw(0xa87900),
    brown = raw(0x653700),
    brownish = raw(0x9c6d57),
    bruise = raw(0x7e4071),
    bubblegum = raw(0xff6cb5),
    buff = raw(0xfef69e),
    burgundy = raw(0x610023),
    burple = raw(0x6832e3),
    butter = raw(0xffff81),
    butterscotch = raw(0xfdb147),
    camel = raw(0xc69f59),
    camo = raw(0x7f8f4e),
    canary = raw(0xfdff63),
    caramel = raw(0xaf6f09),
    carmine = raw(0x9d0216),
    carnation = raw(0xfd798f),
    celadon = raw(0xbefdb7),
    celery = raw(0xc1fd95),
    cement = raw(0xa5a391),
    cerise = raw(0xde0c62),
    cerulean = raw(0x0485d1),
    charcoal = raw(0x343837),
    chartreuse = raw(0xc1f80a),
    cherry = raw(0xcf0234),
    chestnut = raw(0x742802),
    chocolate = raw(0x3d1c02),
    cinnamon = raw(0xac4f06),
    claret = raw(0x680018),
    clay = raw(0xb66a50),
    cobalt = raw(0x1e488f),
    cocoa = raw(0x875f42),
    coffee = raw(0xa6814c),
    copper = raw(0xb66325),
    coral = raw(0xfc5a50),
    cornflower = raw(0x6a79f7),
    cranberry = raw(0x9e003a),
    cream = raw(0xffffc2),
    creme = raw(0xffffb6),
    crimson = raw(0x8c000f),
    custard = raw(0xfffd78),
    cyan = raw(0x00ffff),
    dandelion = raw(0xfedf08),
    dark = raw(0x1b2431),
    darkblue = raw(0x030764),
    darkgreen = raw(0x054907),
    denim = raw(0x3b638c),
    desert = raw(0xccad60),
    diarrhea = raw(0x9f8303),
    dirt = raw(0x8a6e45),
    drab = raw(0x828344),
    dusk = raw(0x4e5481),
    dust = raw(0xb2996e),
    earth = raw(0xa2653e),
    ecru = raw(0xfeffca),
    eggplant = raw(0x380835),
    eggshell = raw(0xffffd4),
    emerald = raw(0x01a049),
    evergreen = raw(0x05472a),
    fawn = raw(0xcfaf7b),
    fern = raw(0x63a950),
    forest = raw(0x0b5509),
    fuchsia = raw(0xed0dd9),
    gold = raw(0xdbb40c),
    golden = raw(0xf5bf03),
    goldenrod = raw(0xfac205),
    grape = raw(0x6c3461),
    grapefruit = raw(0xfd5956),
    grass = raw(0x5cac2d),
    green = raw(0x15b01a),
    greenblue = raw(0x23c48b),
    greenish = raw(0x40a368),
    grey = raw(0x929591),
    greyblue = raw(0x77a1b5),
    greyish = raw(0xa8a495),
    gunmetal = raw(0x536267),
    hazel = raw(0x8e7618),
    heather = raw(0xa484ac),
    heliotrope = raw(0xd94ff5),
    ice = raw(0xd6fffa),
    indigo = raw(0x380282),
    iris = raw(0x6258c4),
    ivory = raw(0xffffcb),
    jade = raw(0x1fa774),
    khaki = raw(0xaaa662),
    kiwi = raw(0x9cef43),
    lavender = raw(0xc79fef),
    leaf = raw(0x71aa34),
    leather = raw(0xac7434),
    lemon = raw(0xfdff52),
    lichen = raw(0x8fb67b),
    lightblue = raw(0x7bc8f6),
    lightgreen = raw(0x76ff7b),
    lilac = raw(0xcea2fd),
    liliac = raw(0xc48efd),
    lime = raw(0xaaff32),
    lipstick = raw(0xd5174e),
    magenta = raw(0xc20078),
    mahogany = raw(0x4a0100),
    maize = raw(0xf4d054),
    mango = raw(0xffa62b),
    manilla = raw(0xfffa86),
    marigold = raw(0xfcc006),
    marine = raw(0x042e60),
    maroon = raw(0x650021),
    mauve = raw(0xae7181),
    melon = raw(0xff7855),
    merlot = raw(0x730039),
    midnight = raw(0x03012d),
    mint = raw(0x9ffeb0),
    mocha = raw(0x9d7651),
    moss = raw(0x769958),
    mud = raw(0x735c12),
    mulberry = raw(0x920a4e),
    mushroom = raw(0xba9e88),
    mustard = raw(0xceb301),
    navy = raw(0x01153e),
    ocean = raw(0x017b92),
    ocher = raw(0xbf9b0c),
    ochre = raw(0xbf9005),
    ocre = raw(0xc69c04),
    olive = raw(0x6e750e),
    orange = raw(0xf97306),
    orangeish = raw(0xfd8d49),
    orangered = raw(0xfe420f),
    orangish = raw(0xfc824a),
    orchid = raw(0xc875c4),
    pale = raw(0xfff9d0),
    parchment = raw(0xfefcaf),
    pea = raw(0xa4bf20),
    peach = raw(0xffb07c),
    pear = raw(0xcbf85f),
    periwinkle = raw(0x8e82fe),
    perrywinkle = raw(0x8f8ce7),
    petrol = raw(0x005f6a),
    pine = raw(0x2b5d34),
    pink = raw(0xff81c0),
    pinkish = raw(0xd46a7e),
    pinky = raw(0xfc86aa),
    pistachio = raw(0xc0fa8b),
    plum = raw(0x580f41),
    poo = raw(0x8f7303),
    poop = raw(0x7f5e00),
    puce = raw(0xa57e52),
    puke = raw(0xa5a502),
    pumpkin = raw(0xe17701),
    purple = raw(0x7e1e9c),
    purpleish = raw(0x98568d),
    purpley = raw(0x8756e4),
    purplish = raw(0x94568c),
    purply = raw(0x983fb2),
    putty = raw(0xbeae8a),
    raspberry = raw(0xb00149),
    red = raw(0xe50000),
    reddish = raw(0xc44240),
    rosa = raw(0xfe86a4),
    rose = raw(0xcf6275),
    rouge = raw(0xab1239),
    royal = raw(0x0c1793),
    ruby = raw(0xca0147),
    russet = raw(0xa13905),
    rust = raw(0xa83c09),
    saffron = raw(0xfeb209),
    sage = raw(0x87ae73),
    salmon = raw(0xff796c),
    sand = raw(0xe2ca76),
    sandstone = raw(0xc9ae74),
    sandy = raw(0xf1da7a),
    sapphire = raw(0x2138ab),
    scarlet = raw(0xbe0119),
    sea = raw(0x3c9992),
    seafoam = raw(0x80f9ad),
    seaweed = raw(0x18d17b),
    sepia = raw(0x985e2b),
    shamrock = raw(0x01b44c),
    shit = raw(0x7f5f00),
    sienna = raw(0xa9561e),
    silver = raw(0xc5c9c7),
    sky = raw(0x82cafc),
    slate = raw(0x516572),
    snot = raw(0xacbb0d),
    spearmint = raw(0x1ef876),
    spruce = raw(0x0a5f38),
    squash = raw(0xf2ab15),
    steel = raw(0x738595),
    stone = raw(0xada587),
    straw = raw(0xfcf679),
    strawberry = raw(0xfb2943),
    sunflower = raw(0xffc512),
    swamp = raw(0x698339),
    tan = raw(0xd1b26f),
    tangerine = raw(0xff9408),
    taupe = raw(0xb9a281),
    tea = raw(0x65ab7c),
    teal = raw(0x029386),
    tealish = raw(0x24bca8),
    terracota = raw(0xcb6843),
    terracotta = raw(0xca6641),
    tomato = raw(0xef4026),
    topaz = raw(0x13bbaf),
    toupe = raw(0xc7ac7d),
    turquoise = raw(0x06c2ac),
    twilight = raw(0x4e518b),
    ultramarine = raw(0x2000b1),
    umber = raw(0xb26400),
    velvet = raw(0x750851),
    vermillion = raw(0xf4320c),
    violet = raw(0x9a0eea),
    viridian = raw(0x1e9167),
    vomit = raw(0xa2a415),
    watermelon = raw(0xfd4659),
    wheat = raw(0xfbdd7e),
    white = raw(0xffffff),
    wine = raw(0x80013f),
    wintergreen = raw(0x20f986),
    wisteria = raw(0xa87dc2),
    yellow = raw(0xffff14),
    yellowgreen = raw(0xbbf90f),
    yellowish = raw(0xfaee66),

    _,

    fn raw(col: u24) u32 {
        const wcol: u32 = @byteSwap(col);
        return 0xFF000000 | wcol;
    }

    pub fn from_rgb_f(col: math.Vec3) Color {
        _ = col;
        return 0xFF000000;
    }

    pub fn from_rgb_i(r: u8, g: u8, b: u8) Color {
        const ri: u32 = @as(u32, r) << 16;
        const gi: u32 = @as(u32, g) << 8;
        const bi: u32 = @as(u32, b) << 0;
        return @enumFromInt(ri | gi | bi | 0xFF000000);
    }

    pub fn to_vec4(self: Color) math.Vec4 {
        const i = @intFromEnum(self);
        const ri: u32 = (i >> 0) & 0xFF;
        const gi: u32 = (i >> 8) & 0xFF;
        const bi: u32 = (i >> 16) & 0xFF;
        const ai: u32 = (i >> 24) & 0xFF;
        const rf: f32 = @floatFromInt(ri);
        const gf: f32 = @floatFromInt(gi);
        const bf: f32 = @floatFromInt(bi);
        const af: f32 = @floatFromInt(ai);

        return math.Vec4{ rf / 255.0, gf / 255.0, bf / 255.0, af / 255.0 };
    }

    pub fn format(self: Color, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("Color({x:0>8})", .{@intFromEnum(self)});
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
