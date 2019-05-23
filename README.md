# JSON/SJSON parser - Public Domain
In-place JSON/SJSON parser implemented in a single header file. No memory allocation, all operations are done in-place. The latest source code is always available at https://github.com/mjansson/json

Created by Mattias Jansson ([@maniccoder](https://twitter.com/maniccoder))

This library is put in the public domain; you can redistribute it and/or modify it without any restrictions.

# Overview

Small in-place JSON parser without any allocation. Entry points for both
standard JSON and simplified JSON data parsing. All character data must be
in UTF-8 format.

Strings are not automatically unescaped. Use json_unescape/json_escape to
perform unescaping and espacing of strings. Unescaping can be done in-place
to avoid memory allocations.

Simplified JSON as parsed by this library has the following differences
from standard JSON:
- The equal sign = is used to define key-value pairs instead of the colon :
- Quotes around string keys in key-value pairs are optional, unless you need
the key to contain either whitespace or the equal sign =
- Commas are optional in object and array definitions
- Each SJSON file is always interpreted as a definition for a single object.
You can think of this as an implicit set of curly quotes { ... } that surround
the contents of the file

Requires size_t, bool and memcmp to be declared prior to including this
header. Headers are only included  when compiled as the minimal test case.

To compile the minimal test case, use
gcc -D JSON_TEST -x c --std=c99 json.h

Kudos to Niklas Gray for SJSON syntax,
http://bitsquid.blogspot.se/2009/10/simplified-json-notation.html

# Usage

	json_size_t
	json_parse(const char* buffer, json_size_t size,
	           struct json_token_t* tokens, json_size_t capacity);

	static json_size_t
	sjson_parse(const char* buffer, json_size_t size,
	            struct json_token_t* tokens, json_size_t capacity);

Parse the given memory buffer as JSON or SJSON into the given token array of
given capacity. Returns the number of parsed tokens, which can be greater
than the supplied capacity to indicate the need for a larger array.

String identifiers and values are not unescaped. This must be performed manually
on each string of interest.

	static json_size_t
	json_unescape(char* buffer, json_size_t capacity, const char* string, json_size_t length);

Unescape a JSON identifier or value string. Buffer can be
pointing to same memory area as string (in-place unescaping).

	static json_size_t
	json_escape(char* buffer, json_size_t capacity, const char* string, json_size_t length);

Escape a JSON identifier or value string. Buffer can NOT be
pointing to same memory area as string.

	static bool
	json_string_equal(const char* rhs, size_t rhs_length, const char* lhs, size_t lhs_length);

Utility function to do bounded string compare.
