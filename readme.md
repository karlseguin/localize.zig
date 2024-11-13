ICU Message Format for Zig

Very basic and preliminary support for parsing and rendering ICU message formats.

```zig
// meant to be re-used to parse multiple messages
var parser = try localize.Parser.init(allocator, .{});
defer parser.deinit();


// can be kept around for a long time
// can outlive the parser.
const msg = try parser.parseMessage("hello {name}!");
defer msg.deinit();

// rendering is thread-safe
try msg.render(some_writer, .{.name = "Leto"});
```

Currently only supports:

* variables
* plural
    * =0 or zero
    * =1 or one
    * other
