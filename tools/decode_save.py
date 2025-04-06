import pprint
import sys
import struct
import collections

Component = collections.namedtuple ('Component', ['size', 'name', 'enum_info', 'struct_info'])
EnumInfo = collections.namedtuple ('EnumInfo', ['tag_type', 'values'])
StructInfo = collections.namedtuple ('StructInfo', ['fields'])
Field = collections.namedtuple ('Field', ['name', 'offset', 'size', 'array_len', 'kind', 'type_id'])

strings_data = b""

all_kinds = {}
all_components = {}
all_entities = {}
all_recycled = {}
all_old_recycled = {}

def read_u32 (data):
    return struct.unpack ("<I", data)[0]

def read_int (data, offset):
    if data[offset] & 0x80 == 0x00:
        return struct.unpack (">B", data[offset:offset+1])[0], offset+1
    elif data[offset] & 0xC0 == 0x80:
        return struct.unpack (">H", data[offset:offset+2])[0] & 0x3FFF, offset+2
    elif data[offset] & 0xE0 == 0xC0:
        return struct.unpack (">I", data[offset:offset+4])[0] & 0x1FFF_FFFF, offset+4
    elif data[offset] == 0xF8:
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

def output_value (kind, field_data, type_id = None):
    if kind == 'bool':
        value = struct.unpack ("<B", field_data)[0] != 0
        print (f"{value}", end = '')
    elif kind == 'u8':
        value = struct.unpack ("<B", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'u16':
        value = struct.unpack ("<H", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'u32':
        value = struct.unpack ("<I", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'u64':
        value = struct.unpack ("<Q", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'u128':
        values = struct.unpack ("<QQ", field_data)
        value = values[1] << 64 | values[0]
        print (f"{value:x} ({field_data})", end = '')
    elif kind == 'i8':
        value = struct.unpack ("<b", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'i16':
        value = struct.unpack ("<h", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'i32':
        value = struct.unpack ("<i", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'i64':
        value = struct.unpack ("<q", field_data)[0]
        print (f"{value}", end = '')
    elif kind == 'f32':
        value = struct.unpack ("<f", field_data)[0]
        print (f"{value:0.3f}", end = '')
    elif kind == 'f64':
        value = struct.unpack ("<d", field_data)[0]
        print (f"{value:0.3f}", end = '')
    elif kind == 'Entity':
        value = struct.unpack ("<I", field_data)[0]
        print (f"E({value:08x})", end = '')
    elif kind == 'Vec2':
        x,y = struct.unpack ("<ff", field_data)
        print (f"Vec2 ({x:0.3f},{y:0.3f})", end = '')
    elif kind == 'Component':
        component = all_components[type_id]
        if component.enum_info:
            info = component.enum_info
            tag_type = all_kinds[info.tag_type]
            if tag_type == 'u8':
                value = struct.unpack ('<B', field_data)[0]
                try:
                    name = info.values[value]
                except:
                    name = "?"
            elif tag_type == 'u16':
                value = struct.unpack ('<H', field_data)[0]
                try:
                    name = info.values[value]
                except:
                    name = "?"
            elif tag_type == 'u32':
                value = struct.unpack ('<I', field_data)[0]
                try:
                    name = info.values[value]
                except:
                    name = "?"
            print (f".{name}", end = '')
        else:
            print (f"{kind} {field_data}", end = '')
    else:
        print (f"{kind} {field_data}", end = '')

def print_component (typeid, data):
    component = all_components[typeid]
    if component.enum_info:
        info = component.enum_info
        tag_type = all_kinds[info.tag_type]
        if tag_type == 'u8':
            value = struct.unpack ('<B', data)[0]
            name = info.values[value]
        elif tag_type == 'u16':
            value = struct.unpack ('<H', data)[0]
            name = info.values[value]
        elif tag_type == 'u32':
            value = struct.unpack ('<I', data)[0]
            name = info.values[value]
        print (f"  {component.name} = .{name}")
    elif component.struct_info:
        print (f"  {component.name}")
        fields = component.struct_info
        for field in fields:
            field_data = data[field.offset:field.offset + field.size]
            if field.array_len == 1:
                kind = all_kinds[field.kind]
                print (f"    .{field.name} = ", end = '')
                output_value (kind, field_data)
                print ("")
            else:
                print (f"    .{field.name} = [", end = '')
                field_size = field.size // field.array_len
                for i in range (field.size // field_size):
                    value_data = field_data[i * field_size : (i+1) * field_size]
                    if i > 0:
                        print (f", ", end = '')
                    output_value (all_kinds[field.kind], value_data, type_id = field.type_id)
                print ("]")

def main ():
    global strings_data
    global all_recycled
    global all_old_recycled
    global all_kinds
    global all_components
    global all_entities

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
            offset += 4
            strings_length = read_u32 (content[strings_offset:strings_offset+4])
            tag = read_tag (content[strings_offset+4:strings_offset+8])
            assert (tag == 'STR:')
            strings_data = content[strings_offset+8 : strings_offset+strings_length]
            while offset < next_offset:
                kind, offset = read_int (content, offset)
                kind_name, offset = read_string (content, offset)
                all_kinds[kind] = kind_name
            pprint.pprint (all_kinds)
        elif tag == 'COMP':
            typeid, offset = read_int (content, offset)
            name, offset = read_string (content, offset)
            size, offset = read_int (content, offset)
            info_kind, offset = read_int (content, offset)
            # print (f"Component {typeid:08x} {name} ({size} bytes) ", end = '')
            if info_kind == 0: # struct
                # print ("struct")
                fields = []
                num_fields, offset = read_int (content, offset)
                for i in range (num_fields):
                    field_name, offset = read_string (content, offset)
                    field_offset, offset = read_int (content, offset)
                    field_size, offset = read_int (content, offset)
                    field_array_len, offset = read_int (content, offset)
                    field_type_id, offset = read_int (content, offset)
                    field_kind, offset = read_int (content, offset)
                    # print (f"  {field_name} {field_offset} {field_size} {field_kind}")
                    fields.append (Field (name = field_name, offset = field_offset, size = field_size, array_len = field_array_len, kind = field_kind, type_id = field_type_id))
                component = Component (name = name, size = size, struct_info = fields, enum_info = None)
                all_components[typeid] = component
            elif info_kind == 1: # enum
                # print ("enum")
                values = {}
                tag_type, offset = read_int (content, offset)
                num_values, offset = read_int (content, offset)
                for i in range (num_values):
                    enum_value, offset = read_int (content, offset)
                    enum_name, offset = read_string (content, offset)
                    values[enum_value] = enum_name
                    # print (f"  {enum_name} = {enum_value}")
                component = Component (name = name, size = size, enum_info = EnumInfo (tag_type = tag_type, values = values), struct_info = None)
                all_components[typeid] = component
        elif tag == 'ENTT':
            entity = read_u32 (content[offset:offset+4])
            entity_data = {}
            offset += 4
            while offset < next_offset:
                typeid, offset = read_int (content, offset) 
                length, offset = read_int (content, offset) 
                data = content[offset:offset+length]
                offset += length
                entity_data[typeid] = data
            all_entities[entity] = entity_data
        elif tag == 'STR:':
            pass
        elif tag == 'RCYC':
            while offset < next_offset:
                idx, offset = read_int (content, offset)
                all_recycled[idx] = True
        elif tag == 'OLDR':
            while offset < next_offset:
                idx, offset = read_int (content, offset)
                all_old_recycled[idx] = True
        offset = next_offset

    for entity in all_entities:
        entity_data = all_entities[entity]
        if entity & 0xFFFFFF in all_recycled:
            print (f"Entity {entity:08x} recycled")
        elif entity & 0xFFFFFF in all_old_recycled:
            print (f"Entity {entity:08x} old_recycled")
        else:
            print (f"Entity {entity:08x} alive")
        for typeid in entity_data:
            data = entity_data[typeid]
            print_component (typeid, data)

if __name__ == "__main__":
    main ();
