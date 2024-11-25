ICU Message Format for Zig

Very basic support for parsing and rendering ICU message formats.

## Install
1) Add localize.zig as a dependency in your `build.zig.zon`:

```bash
zig fetch --save git+https://github.com/karlseguin/localize.zig#master
```

2) In your `build.zig`, add the `localize` module as a dependency you your program:

```zig
const localize = b.dependency("localize", .{
    .target = target,
    .optimize = optimize,
});

// the executable from your call to b.addExecutable(...)
exe.root_module.addImport("localize", localize.module("localize"));
```


## Usage
First, create a `Resource`:

```zig
var resource = try localize.Resource(Locales).init(allocator);
defer resource.deinit();

const Locales = enum {
    en,
    fr,
};
```

This will likely be a long-lived object. Next, create a parser for each locale and add messages:

```zig
var parser = try resource.parser(.en, .{});
defer parser.deinit();

// loop over entries in a file, or something
// localize.zig currently doesn't "read" files

try parser.add("string_len_min", \\ must be at least {min}
    \\ {min, plural,
    \\  =1 {character}
    \\  other {characters}
    \\  }
    \\ long
);
```

Once all messages have been loaded, you can use `resource.write` to write a localized message:

```zig
try resource.write(writer, .en, "string_len_min", .{.min = 6});
```

The `write` method is thread-safe.

In cases where you'll be generating multiple message for a single locale, you can first get a <code>Locale</code> and then use its thread-safe write:</

```zig
// you very likely have logic in your code that makes it so that
// this could never return null
var locale = resource.getLocale(.en) orelse unreachable;

locale.write("string_len_min", .{.min = 6});
```

## Limited Functionality
Currently, this only supports:

* variables
* plural
    * =0 or zero
    * =1 or one
    * other
