import pprint
import sys
import struct

strings_data = b""

def read_u32 (data):
    return struct.unpack (">I", data)[0]

def read_u64 (data):
    return struct.unpack (">Q", data)[0]

def read_int (data, offset):
    if data[offset] & 0x80 == 0x00:
        return struct.unpack (">B", data[offset:offset+1])[0], offset+1
    elif data[offset] & 0xC0 == 0x80:
        return struct.unpack (">H", data[offset:offset+2])[0] & 0x3FFF, offset+2
    elif data[offset] & 0xE0 == 0xC0:
        return struct.unpack (">I", data[offset:offset+4])[0] & 0x1FFF_FFFF, offset+4
    elif data[offset] == 0xF0:
        return struct.unpack (">Q", data[offset+1:offset+9])[0], offset+9
    else:
        return (0, 1)

def read_string (data, offset):
    global strings_data

    string_offset, offset = read_int (data, offset);
    string_length, offset = read_int (data, offset);
    return strings_data[string_offset:string_offset+string_length].decode ('utf8'), offset

def read_tag (data):
    tag = struct.unpack ("cccc", data)
    return (tag[0] + tag[1] + tag[2] + tag[3]).decode ('utf8')

def process_strings (content):
    pprint.pprint (data)
    length = read_u32 (content[offset:offset+4])
    next_offset = offset + length
    offset += 4
    tag = read_tag (content[offset:offset+4])
    offset += 4
    print (f"{tag} {length}")

def main ():
    global strings_data

    fp = open (sys.argv[1], "rb")
    content = fp.read ()
    fp.close ()

    offset = 0
    while offset < len (content):
        length = read_u32 (content[offset:offset+4])
        next_offset = offset + length
        offset += 4
        tag = read_tag (content[offset:offset+4])
        offset += 4
        if tag == 'HEAD':
            strings_offset = read_u32 (content[offset:offset+4])
            strings_length = read_u32 (content[strings_offset:strings_offset+4])
            tag = read_tag (content[strings_offset+4:strings_offset+8])
            assert (tag == 'STR:')
            strings_data = content[strings_offset+8 : strings_offset+strings_length]
        elif tag == 'COMP':
            typeid, offset = read_int (content, offset)
            name, offset = read_string (content, offset)
            size, offset = read_int (content, offset)
            info_kind, offset = read_int (content, offset)
            print (f"Component {name} ({size} bytes) ", end = '')
            if info_kind == 0: # struct
                print ("struct")
                num_fields, offset = read_int (content, offset)
                for i in range (num_fields):
                    field_name, offset = read_string (content, offset)
                    field_offset, offset = read_int (content, offset)
                    field_size, offset = read_int (content, offset)
                    field_kind, offset = read_int (content, offset)
                    field_component, offset = read_int (content, offset)
                    print (f"  {field_name} {field_offset} {field_size} {field_kind} {field_component}")
            elif info_kind == 1: # enum
                print ("enum")
                tag_type, offset = read_int (content, offset)
                num_values, offset = read_int (content, offset)
                for i in range (num_values):
                    enum_value, offset = read_int (content, offset)
                    enum_name, offset = read_string (content, offset)
                    print (f"  {enum_name} = {enum_value}")
        elif tag == 'ENTT':
            entity = read_u32 (content[offset:offset+4])
            print (f"Entity {entity:08x}")
            offset += 4
            while offset < next_offset:
               typeid, offset = read_int (content, offset) 
               length, offset = read_int (content, offset) 
               data = content[offset:offset+length]
               offset += length
               print (f"  {typeid:08x} {data}")
        elif tag == 'STR:':
            pass
        offset = next_offset

if __name__ == "__main__":
    main ();
