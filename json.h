/* json.h  -  JSON/SJSON parser  -  Public Domain  -  2016 Mattias Jansson / Rampant Pixels
 *
 * This library provides a in-place JSON/SJSON parser in C99.
 * The latest source code is always available at:
 *
 * https://github.com/rampantpixels/json
 *
 * This library is put in the public domain; you can redistribute
 * it and/or modify it without any restrictions.
 */

#pragma once

/*! \file json.h
\brief JSON/SJSON parser

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

Kudos to Niklas Gray for SJSON syntax,
http://bitsquid.blogspot.se/2009/10/simplified-json-notation.html
*/

// Types

/*! Base size type. Change this to reduce token storage footprint. */
typedef size_t json_size_t;

/*! JSON token type */
enum json_type_t {
	/*! Invalid type */
	JSON_UNDEFINED = 0,
	/*! Object */
	JSON_OBJECT,
	/*! Array */
	JSON_ARRAY,
	/*! String */
	JSON_STRING,
	/*! Primitive */
	JSON_PRIMITIVE
};

/*! JSON token. The token points into the parsed data buffer using absolute offsets
from start of buffer */
struct json_token_t {
	/*! Identifier string offset */
	json_size_t id;
	/*! Length of identifier string. 0 if no identifier string */
	json_size_t id_length;
	/*! Value string offset */
	json_size_t value;
	/*! Length of value string. 0 if no or empty value string */
	json_size_t value_length;
	/*! Child token index in token array. 0 if no child token */
	json_size_t child;
	/*! Sibling token index in token array. 0 if no sibling token */
	json_size_t sibling;
	/*! Token type */
	enum json_type_t type;
};

// Interface

/*! Parse a JSON blob. Number of parsed tokens can be greater than the supplied
capacity to indicate the need for additional capacity for a full parse. Note that
string identifiers and values are in escaped form.
\param buffer Data buffer
\param size Size of data buffer
\param tokens Token array
\param capacity Capacity of token array (number of tokens)
\return Number of parsed tokens, 0 if error */
static json_size_t
json_parse(const char* buffer, json_size_t size,
           struct json_token_t* tokens, json_size_t capacity);

/*! Parse a simplified JSON blob. Number of parsed tokens can be greater than the supplied
capacity to indicate the need for additional capacity for a full parse. Not that
string identifiers and values are in escaped form.
\param buffer Data buffer
\param size Size of data buffer
\param tokens Token array
\param capacity Capacity of token array (number of tokens)
\return Number of parsed tokens, 0 if error */
static json_size_t
sjson_parse(const char* buffer, json_size_t size,
            struct json_token_t* tokens, json_size_t capacity);

/*! Function to unescape a JSON identifier or value string. Buffer can be
pointing to same memory area as string (in-place unescaping).
\param buffer Output buffer
\param capacity Capacity of output buffer
\param string Input string identifier or value
\param length Length of input string
\return Length of unescaped string in buffer */
static json_size_t
json_unescape(char* buffer, json_size_t capacity, const char* string, json_size_t length);

/*! Function to escape a JSON identifier or value string
\param buffer Output buffer
\param capacity Capacity of output buffer
\param string Input string identifier or value
\param length Length of input string
\return Escaped string in buffer */
static json_size_t
json_escape(char* buffer, json_size_t capacity, const char* string, json_size_t length);

/*! Utility function to do bounded string compare
\param rhs First string
\param rhs_length Length of first string in bytes
\param lhs Second string
\param lhs_length Length of second string in bytes
\return true if strings are equal, false if not */
static bool
json_string_equal(const char* rhs, size_t rhs_length, const char* lhs, size_t lhs_length);

/*! \def JSON_STRING_CONST
\brief Utility string macro for both data and length */
/*! Expands to two arguments (data and length) of a constant string expression, like <CODE>JSON_STRING_CONST("foobar")</CODE>.
Useful with json_string_equal function: <CODE>json_string_equal(myptr, mylength, JSON_STRING_CONST("foobar"))</CODE>.
Be aware that it evaluates the s expression twice. */
#define JSON_STRING_CONST(s) (s), (sizeof((s))-1)


// Implementation

//! Identifier of invalid position or index
#define JSON_INVALID_POS ((json_size_t)-1)

static struct json_token_t*
json_get_token(struct json_token_t* tokens, json_size_t capacity, json_size_t index) {
	return index < capacity ? tokens + index : 0;
}

static bool
json_is_valid_token(struct json_token_t* tokens, json_size_t capacity, json_size_t index) {
	struct json_token_t* token = json_get_token(tokens, capacity, index);
	return token ? (token->type != JSON_UNDEFINED) : true;
}

static void
json_set_token_primitive(struct json_token_t* tokens, json_size_t capacity, json_size_t current,
                         json_type_t type, json_size_t value, json_size_t value_length) {
	struct json_token_t* token = json_get_token(tokens, capacity, current);
	if (token) {
		token->type = type;
		token->child = 0;
		token->sibling = 0;
		token->value = value;
		token->value_length = value_length;
	}
}

static void
json_set_token_complex(struct json_token_t* tokens, json_size_t capacity, json_size_t current,
                       json_type_t type) {
	struct json_token_t* token = json_get_token(tokens, capacity, current);
	if (token) {
		token->type = type;
		token->child = current + 1;
		token->sibling = 0;
		token->value = 0;
		token->value_length = 0;
	}
}

static void
json_set_token_id(struct json_token_t* tokens, json_size_t capacity, json_size_t current,
                  json_size_t id, json_size_t id_length) {
	struct json_token_t* token = json_get_token(tokens, capacity, current);
	if (token) {
		token->id = id;
		token->id_length = id_length;
	}
}

static bool
json_is_whitespace(char c) {
	return (c == ' ')  || (c == '\t') || (c == '\n') || (c == '\r');
}

static bool
json_is_token_delimiter(char c) {
	return json_is_whitespace(c) || (c == ']')  || (c == '}') || (c == ',');
}

static json_size_t
json_skip_whitespace(const char* buffer, json_size_t length, json_size_t pos) {
	while (pos < length) {
		if (!json_is_whitespace(buffer[pos]))
			return pos;
		++pos;
	}
	return pos;
}

static char
json_hex_char(unsigned char val) {
	if (val < 10)
		return '0' + (char)val;
	else if (val < 16)
		return 'a' + (char)(val - 10);
	return '0';
}

static json_size_t
json_parse_string(const char* buffer, json_size_t length, json_size_t pos, bool key, bool simple) {
	json_size_t start = pos;
	json_size_t esc;
	while (pos < length) {
		char c = buffer[pos];
		if (simple && (json_is_token_delimiter(c) || (key && ((c == '=') || (c == ':')))))
			return pos - start;
		if (c == '"')
			return pos - start;
		++pos;
		if (c == '\\' && (pos < length)) {
			switch (buffer[pos]) {
			// Escaped symbols \X
			case '\"': case '/': case '\\': case 'b':
			case 'f' : case 'r': case 'n' : case 't':
				break;
			// Escaped symbol \uXXXX
			case 'u':
				for (esc = 0; esc < 4 && pos < length; ++esc) {
					++pos;
					if (!((buffer[pos] >= 48 && buffer[pos] <= 57) || // 0-9
					        (buffer[pos] >= 65 && buffer[pos] <= 70) || // A-F
					        (buffer[pos] >= 97 && buffer[pos] <= 102))) // a-f
						return JSON_INVALID_POS;
				}
				break;
			default:
				return JSON_INVALID_POS;
			}
			++pos;
		}
	}
	return simple ? pos - start : JSON_INVALID_POS;
}

static json_size_t
json_parse_number(const char* buffer, json_size_t length, json_size_t pos) {
	json_size_t start = pos;
	bool has_dot = false;
	bool has_digit = false;
	bool has_exp = false;
	while (pos < length) {
		char c = buffer[pos];
		if (json_is_token_delimiter(c))
			break;
		if (c == '-') {
			if (start != pos)
				return JSON_INVALID_POS;
		}
		else if (c == '.') {
			if (has_dot || has_exp)
				return JSON_INVALID_POS;
			has_dot = true;
		}
		else if ((c == 'e') || (c == 'E')) {
			if (!has_digit || has_exp)
				return JSON_INVALID_POS;
			has_exp = true;
			if ((pos + 1) < length) {
				if ((buffer[pos+1] == '+') || (buffer[pos+1] == '-'))
					++pos;
			}
		}
		else if ((c < '0') || (c > '9'))
			return JSON_INVALID_POS;
		else
			has_digit = true;
		++pos;
	}
	return has_digit ? (pos - start) : JSON_INVALID_POS;
}

static json_size_t
json_parse_object(const char* buffer, json_size_t length, json_size_t pos,
                  struct json_token_t* tokens, json_size_t capacity, json_size_t* current, bool simple);

static json_size_t
json_parse_value(const char* buffer, json_size_t length, json_size_t pos,
                 struct json_token_t* tokens, json_size_t capacity, json_size_t* current, bool simple);

static json_size_t
json_parse_array(const char* buffer, json_size_t length, json_size_t pos,
                 struct json_token_t* tokens, json_size_t capacity, json_size_t* current, bool simple);

static json_size_t
json_parse_object(const char* buffer, json_size_t length, json_size_t pos,
                  struct json_token_t* tokens, json_size_t capacity, json_size_t* current, bool simple) {
	struct json_token_t* token;
	json_size_t string;
	bool simple_string;
	json_size_t last = 0;

	pos = json_skip_whitespace(buffer, length, pos);
	while (pos < length) {
		char c = buffer[pos++];

		switch (c) {
		case '}':
			if (last && !json_is_valid_token(tokens, capacity, last))
				return JSON_INVALID_POS;
			return pos;

		case ',':
			if (!last || !json_is_valid_token(tokens, capacity, last))
				return JSON_INVALID_POS;
			token = json_get_token(tokens, capacity, last);
			if (token)
				token->sibling = *current;
			last = 0;
			pos = json_skip_whitespace(buffer, length, pos);
			break;

		case '"':
		default:
			if (last)
				return JSON_INVALID_POS;
			if (c != '"') {
				if (!simple)
					return JSON_INVALID_POS;
				simple_string = true;
				--pos;
			}
			else {
				simple_string = false;
			}

			string = json_parse_string(buffer, length, pos, true, simple_string);
			if (string == JSON_INVALID_POS)
				return JSON_INVALID_POS;

			last = *current;
			json_set_token_id(tokens, capacity, *current, pos, string);
			//Skip terminating '"' (optional for simplified)
			if (!simple || ((pos + string < length) && (buffer[pos + string] == '"')))
				++string;
			pos += string;

			pos = json_skip_whitespace(buffer, length, pos);
			if ((buffer[pos] != ':') &&
			        (!simple || (buffer[pos] != '=')))
				return JSON_INVALID_POS;
			pos = json_parse_value(buffer, length, pos + 1, tokens, capacity, current, simple);
			pos = json_skip_whitespace(buffer, length, pos);
			if (simple_string && ((pos < length) && (buffer[pos] != ',') && (buffer[pos] != '}'))) {
				token = json_get_token(tokens, capacity, last);
				if (token)
					token->sibling = *current;
				last = 0;
			}
			break;
		}
	}

	return simple ? pos : JSON_INVALID_POS;
}

static json_size_t
json_parse_array(const char* buffer, json_size_t length, json_size_t pos,
                 struct json_token_t* tokens, json_size_t capacity, json_size_t* current, bool simple) {
	struct json_token_t* token;
	json_size_t now;
	json_size_t last = 0;

	pos = json_skip_whitespace(buffer, length, pos);
	if (buffer[pos] == ']')
		return json_skip_whitespace(buffer, length, ++pos);

	while (pos < length) {
		now = *current;
		json_set_token_id(tokens, capacity, now, 0, 0);
		pos = json_parse_value(buffer, length, pos, tokens, capacity, current, simple);
		if (pos == JSON_INVALID_POS)
			return JSON_INVALID_POS;
		if (last) {
			token = json_get_token(tokens, capacity, last);
			if (token)
				token->sibling = now;
		}
		last = now;
		pos = json_skip_whitespace(buffer, length, pos);
		if (buffer[pos] == ',')
			++pos;
		else if (buffer[pos] == ']')
			return ++pos;
		else if (!simple || buffer[pos] == '}')
			return JSON_INVALID_POS;
	}

	return JSON_INVALID_POS;
}

static json_size_t
json_parse_value(const char* buffer, json_size_t length, json_size_t pos,
                 struct json_token_t* tokens, json_size_t capacity, json_size_t* current, bool simple) {
	json_size_t string;
	bool simple_string;

	pos = json_skip_whitespace(buffer, length, pos);
	while (pos < length) {
		char c = buffer[pos++];
		switch (c) {
		case '{':
			json_set_token_complex(tokens, capacity, *current, JSON_OBJECT);
			++(*current);
			pos = json_parse_object(buffer, length, pos, tokens, capacity, current, simple);
			return pos;

		case '[':
			json_set_token_complex(tokens, capacity, *current, JSON_ARRAY);
			++(*current);
			pos = json_parse_array(buffer, length, pos, tokens, capacity, current, simple);
			return pos;

		case '-': case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9': case '.':
			string = json_parse_number(buffer, length, pos - 1);
			if (string == JSON_INVALID_POS)
				return JSON_INVALID_POS;
			json_set_token_primitive(tokens, capacity, *current, JSON_PRIMITIVE, pos - 1, string);
			++(*current);
			return pos + string - 1;

		case 't':
		case 'f':
			if ((c == 't') && (length - pos >= 4) &&
			        (buffer[pos] == 'r') && (buffer[pos+1] == 'u') && (buffer[pos+2] == 'e') &&
			        json_is_token_delimiter(buffer[pos+3])) {
				json_set_token_primitive(tokens, capacity, *current, JSON_PRIMITIVE, pos - 1, 4);
				++(*current);
				return pos + 3;
			}
			if ((c == 'f') && (length - pos >= 5) &&
			        (buffer[pos] == 'a') && (buffer[pos+1] == 'l') && (buffer[pos+2] == 's') &&
			        (buffer[pos+3] == 'e') && json_is_token_delimiter(buffer[pos+4])) {
				json_set_token_primitive(tokens, capacity, *current, JSON_PRIMITIVE, pos - 1, 5);
				++(*current);
				return pos + 4;
			}
			if (!simple)
				return JSON_INVALID_POS;
		//Fall through to string handling

		case '"':
		default:
			if (c != '"') {
				if (!simple)
					return JSON_INVALID_POS;
				simple_string = true;
				--pos;
			}
			else {
				simple_string = false;
			}
			string = json_parse_string(buffer, length, pos, false, simple_string);
			if (string == JSON_INVALID_POS)
				return JSON_INVALID_POS;
			json_set_token_primitive(tokens, capacity, *current, JSON_STRING, pos, string);
			++(*current);
			//Skip terminating '"' (optional for simplified)
			if (!simple_string || ((pos + string < length) && (buffer[pos + string] == '"')))
				++string;
			return pos + string;
		}
	}

	return JSON_INVALID_POS;
}

static json_size_t
json_parse(const char* buffer, json_size_t size, struct json_token_t* tokens,
           json_size_t capacity) {
	json_size_t current = 0;
	json_set_token_id(tokens, capacity, current, 0, 0);
	json_set_token_primitive(tokens, capacity, current, JSON_UNDEFINED, 0, 0);
	if (json_parse_value(buffer, size, 0, tokens, capacity, &current, false) == JSON_INVALID_POS)
		return 0;
	return current;
}

static json_size_t
sjson_parse(const char* buffer, json_size_t size, struct json_token_t* tokens,
            json_size_t capacity) {
	json_size_t current = 0;
	json_size_t pos = json_skip_whitespace(buffer, size, 0);
	if ((pos < size) && (buffer[pos] != '{')) {
		json_set_token_id(tokens, capacity, current, 0, 0);
		json_set_token_complex(tokens, capacity, current, JSON_OBJECT);
		++current;
		if (json_parse_object(buffer, size, pos, tokens, capacity, &current, true) == JSON_INVALID_POS)
			return 0;
		return current;
	}
	if (json_parse_value(buffer, size, pos, tokens, capacity, &current, true) == JSON_INVALID_POS)
		return 0;
	return current;
}

static json_size_t
json_escape(char* buffer, json_size_t capacity, const char* string, json_size_t length) {
	json_size_t i;
	json_size_t outlength = 0;
	for (i = 0; (i < length) && (outlength < capacity); ++i) {
		char c = string[i];
		if ((c == '\"') || (c == '\\')) {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = c;
		}
		else if (c == '\b') {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = 'b';
		}
		else if (c == '\f') {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = 'f';
		}
		else if (c == '\r') {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = 'r';
		}
		else if (c == '\n') {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = 'n';
		}
		else if (c == '\t') {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = 't';
		}
		else if (c < 0x20) {
			buffer[outlength++] = '\\';
			if (outlength < capacity) buffer[outlength++] = 'u';
			if (outlength < capacity) buffer[outlength++] = '0';
			if (outlength < capacity) buffer[outlength++] = '0';
			if (outlength < capacity) buffer[outlength++] = json_hex_char((unsigned char)(c >> 4) & 0xf);
			if (outlength < capacity) buffer[outlength++] = json_hex_char((unsigned char)c & 0xf);
		}
		else {
			buffer[outlength++] = c;
		}
	}
	return outlength;
}

//! Define a bitmask with the given number of bits set to 1
#define JSON_BITMASK(numbits) ((1U << (numbits)) - 1)

static unsigned int
json_get_num_bytes_as_utf8(unsigned int val) {
	if (val >= 0x04000000) return 6;
	else if (val >= 0x00200000) return 5;
	else if (val >= 0x00010000) return 4;
	else if (val >= 0x00000800) return 3;
	else if (val >= 0x00000080) return 2;
	return 1;
}

static json_size_t
json_encode_utf8(char* str, unsigned int val) {
	unsigned int num, j;

	if (val < 0x80) {
		*str = (char)val;
		return 1;
	}

	//Get number of _extra_ bytes
	num = json_get_num_bytes_as_utf8(val) - 1;

	*str++ = (char)((0x80U | (JSON_BITMASK(num) << (7U - num))) |
	                ((val >> (6U * num)) & JSON_BITMASK(6U - num)));
	for (j = 1; j <= num; ++j)
		*str++ = (char)(0x80U | ((val >> (6U * (num - j))) & 0x3F));

	return num + 1;
}

static json_size_t
json_unescape(char* buffer, json_size_t capacity, const char* string, json_size_t length) {
	json_size_t i, j;
	json_size_t outlength = 0;
	unsigned int hexval, numbytes;
	for (i = 0; (i < length) && (outlength < capacity); ++i) {
		char c = string[i];
		if ((c == '\\') && (i + 1 < length)) {
			c = string[++i];
			switch (c) {
			case '\"':
			case '/':
			case '\\':
				buffer[outlength++] = c;
				break;

			case 'b':
				buffer[outlength++] = '\b';
				break;
			case 'f':
				buffer[outlength++] = '\f';
				break;
			case 'r':
				buffer[outlength++] = '\r';
				break;
			case 'n':
				buffer[outlength++] = '\n';
				break;
			case 't':
				buffer[outlength++] = '\t';
				break;

			case 'u':
				if (i + 4 < length) {
					hexval = 0;
					for (j = 0; j < 4; ++j) {
						char val = string[++i];
						unsigned int uival = 0;
						if ((val >= 'a') && (val <= 'f'))
							uival = 10 + (val - 'a');
						else if ((val >= 'A') && (val <= 'F'))
							uival = 10 + (val - 'A');
						else if ((val >= '0') && (val <= '9'))
							uival = val - '0';
						hexval |= uival << (3-j);
					}
					numbytes = json_get_num_bytes_as_utf8(hexval);
					if ((outlength + numbytes) < capacity)
						outlength += json_encode_utf8(buffer + outlength, hexval);
				}
				break;

			default:
				break;
			}
		}
		else {
			buffer[outlength++] = c;
		}
	}
	return outlength;
}

#include <string.h>

static bool
json_string_equal(const char* rhs, size_t rhs_length, const char* lhs, size_t lhs_length) {
	if (rhs_length && (lhs_length == rhs_length)) {
		return (memcmp(rhs, lhs, rhs_length) == 0);
	}
	return (!rhs_length && !lhs_length);
}
