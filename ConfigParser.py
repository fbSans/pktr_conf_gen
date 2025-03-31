from PTConfigDataStrucures import *
from ConfigToken import *
from ConfigLexer import *


#################################### Helper Functions ################################################################

#used to make assertions uppon token types and/or values. if the provided argument to the parameter value is None, value won't be checked
def assert_expected_token(actual_token: Token, expected_type: TokenType, expected_value: str | None = None):
    if expected_value is None:
        if not (actual_token.type == expected_type): panic(f"{actual_token.location} Expected token type {expected_type.name}, but found token type {actual_token.type.name}.")
    else:
        if not (actual_token.type == expected_type and actual_token.value == expected_value): 
            panic(f"{actual_token.location} Expected token type {expected_type.name} and value to be {expected_value}, but found token type{actual_token.type.name} and value {actual_token.value}.")


#checks the next token type to be in the list of the expected, if not it will panic, it does not alter the tokens list
def expect_next_token_type(tokens: list[Token], expected_token_types: list[TokenType]):
    if len(tokens) == 0 :
        panic(f"expected to find token type in {[typ.name for typ in expected_token_types]}, but found no more tokens")
    if tokens[0].type not in expected_token_types: 
        panic(f"{tokens[0].location} expected to find token type in {[typ.name for typ in expected_token_types]}, but found {tokens[0].type}")


#checks the next token value to be in the list of the expected, if not it will panic, it does not alter the tokens list
def expect_next_token_value(tokens: list[Token], expected_token_values: list[str]):
    if len(tokens) == 0 :
        panic(f"expected to find token value in {expected_token_values}, but found no more tokens")
    elif tokens[0].value.upper() not in expected_token_values: 
        panic(f"{tokens[0].location}: expected to find token type in {expected_token_values}, but found {tokens[0].value}")


#removes the next comma in  the token list. if there is no comma it will fail, unless it finds an empty list, or closing tokens like }, ], ) 
def remove_next_comma(tokens: list[Token]) -> list[Token]:
    if len(tokens) > 0:
        expect_next_token_type(tokens, [TokenType.COMMA, TokenType.CLOSE_CURLY, TokenType.CLOSE_SQUARE, TokenType.CLOSE_PAREN])
        if tokens[0].type == TokenType.COMMA: _, *tokens = tokens
    return tokens


#checks if property is one of the expected and if it is not redefined, if any of the previous conditions is not met, it crashes. It adds the property in the set of already defined properties
def check_property(parsee_object: Token, property: Token, expected_property_names: list[str], already_found_properties: set[str]):
    if not property.value in expected_property_names:
        panic(f"{property.location} Unexpected property {property.value} for {parsee_object.value}")
    if property.value in already_found_properties: 
        panic(f"{property.location} Redefinition of property {property.value} for {parsee_object.value}")
    already_found_properties.add(property.value)
   

#here simple pair means that the value is a string literal or an identifier, removes the comma in the end if it exists
def next_simple_pair(tokens: list[Token]) -> tuple[Token, Token, list[Token]]:
    expect_next_token_type(tokens, [TokenType.KEYWORD, TokenType.IDENTIFIER])
    key, *tokens = tokens
    expect_next_token_type(tokens, [TokenType.COLON])
    _, *tokens = tokens
    expect_next_token_type(tokens, [TokenType.STRING_LITERAL, TokenType.IDENTIFIER, TokenType.KEYWORD, TokenType.INTEGER_LITERAL])
    if(tokens[0].type == TokenType.KEYWORD): expect_next_token_value(tokens, [Keyword.TRUE.name, Keyword.FALSE.name])
    value, *tokens = tokens
    
    tokens = remove_next_comma(tokens)
    
    return key, value, tokens


################################# IP validation functions ###########################################################
def is_ipv4(str: str) -> bool:
    parts = str.split('.')
    if len(parts) != 4: return False
    for part in parts:
        if not part.isdigit() and not (0 <= int(part) <= 255): return False
    return True

#This function was used to generate the ip list
# def ipv4_mask_from_num(n: int) -> int:
#     if not (0 <= n <= 32): return 0
#     mask = 0
#     for i in range(32):
#         mask *= 2
#         if n > 0:
#             mask += 1
#             n -= 1
#         else:
#             mask += 0
#     return mask

valid_ipv4_masks =  {   
                        0x0,        0x80000000, 0xc0000000, 0xe0000000, 0xf0000000, 0xf8000000, 0xfc000000, 0xfe000000, 
                        0xff000000, 0xff800000, 0xffc00000, 0xffe00000, 0xfff00000, 0xfff80000, 0xfffc0000, 0xfffe0000,
                        0xffff0000, 0xffff8000, 0xffffc000, 0xffffe000, 0xfffff000, 0xfffff800, 0xfffffc00, 0xfffffe00, 
                        0xffffff00, 0xffffff80, 0xffffffc0, 0xffffffe0, 0xfffffff0, 0xfffffff8, 0xfffffffc, 0xfffffffe,
                        0xffffffff,
                    }

def is_ipv4_mask(mask: str):
    if not is_ipv4(mask): return False
    octets = mask.split('.')
    int_mask = 0
    for octet in octets:
        int_mask <<= 8
        int_mask += int(octet)
    return int_mask in valid_ipv4_masks

def is_ipv4_with_mask(str: str) -> bool:
    if len(str.split()) != 2: return False
    ip, mask, *rest = str.split()
    return is_ipv4(ip) and  is_ipv4_mask(mask)
        


def valid_sextect(s: str):
    if len(s) > 4:
        return False
    for c in s:
        if c.lower() not in "0123456789abcdef":
            return False
    return True
    

def is_ipv6(ip: str):
    sextects = []
    if ip.count(':::') > 0:
        return False
    if ip.count('::') > 1:
        return False
    if ip.count('::') == 1:
        parts = ip.split('::')
        sextects.extend(parts[0].split(':'))
        sextects.extend(parts[1].split(':'))
        if(len(sextects) >= 8):
            return False
    else:
        sextects.extend(ip.split(':'))
        if(len(sextects) != 8):
            return False
    for term in sextects:
        if not valid_sextect(term):
            return False
    return True

def is_ipv6_with_mask(ip: str):
    parts = ip.split('/')
    if len(parts) != 2:
        return False
    return is_ipv6(parts[0]) and parts[1].isdigit() and 0 <= int(parts[1]) <= 128


############################################## Generating Functions ##################################################
    
#parses line_vty from the list of tokens
def parse_line_vty(tokens: list[Token]) -> tuple[int, int, list[Token]]:
    found_items: set[str] = set()
    start, end = 0, 0
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [Keyword.LINE_VTY.name])
    header , *tokens = tokens
    expect_next_token_type(tokens, [TokenType.COLON])
    _, *tokens = tokens
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    _, *tokens = tokens
    
    for _ in range(2):
        key, value, tokens = next_simple_pair(tokens)
        assert_expected_token(key, TokenType.KEYWORD)
        check_property(header, key, [Keyword.START.name, Keyword.END.name], found_items)
        match key.value:
            case Keyword.START.name:
               start = int(value.value) 
            case Keyword.END.name:
                end = int(value.value)
            case _:
                #make it fail on porpose to report with the default message
                assert False, "parse_line_vty: Unreachable"

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens

    tokens = remove_next_comma(tokens) 

    return start, end, tokens


def parse_bool(str: str):
    match str.upper():
        case "TRUE":
            return True
        case "FALSE":
            return False
        case _:
            panic(f"{str} cannot be converted into boolean")


def parse_device_config(tokens: list[Token], variables: dict[str,int|str|list|dict]) -> tuple[CONFIG_INFO, list[Token], dict[str,int|str|list|dict]]:
    found_items : set[str] = set()

    config_expected_item_names = [
                        Keyword.HOSTNAME.name, 
                        Keyword.DOMAIN_NAME.name, 
                        Keyword.PASSWORD.name, 
                        Keyword.MOTD.name, 
                        Keyword.LINE_CONSOLE.name, 
                        Keyword.LINE_VTY.name, 
                        Keyword.ENABLE_SSH.name , 
                        Keyword.SSH_PASSWORD.name,
                        Keyword.SSH_KEY_SIZE.name,
                    ]

    #start parsing
    #strinping the header CONFIG | DEFAULT_CONFIG : {
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [Keyword.DEFAULT_CONFIG.name.upper(), Keyword.CONFIG.name.upper()])
    config_token , *tokens = tokens

    expect_next_token_type(tokens, [TokenType.COLON])
    _ , *tokens = tokens
   
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly , *tokens = tokens
   

    #non-default config will have its fieds initialized with None
    #The fields that are not changed to a not None value will recieve the DEFAULT_CONFIG fields
    if config_token.value != Keyword.DEFAULT_CONFIG.name:
        config = CONFIG_INFO(None, None, None, None, None, None, None, None, None)
    else:
        config = CONFIG_INFO() # DEFAULT_CONFIG is initialized with reasanable values

    #lack of do while loop ;)
    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {config_token.name}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        #line vty an object
        if tokens[0].type == TokenType.KEYWORD and tokens[0].value == Keyword.LINE_VTY.name:
            start, end, tokens = parse_line_vty(tokens)
            config.line_vty = start, end
        else:
            #to parse simple values
            expect_next_token_type(tokens, [TokenType.KEYWORD])
            key, value, tokens = next_simple_pair(tokens)
        
            check_property(config_token, key, config_expected_item_names, found_items)

            if value.type == TokenType.IDENTIFIER:
                #TODO: Implement support for identifier names
                panic(f"{value.location} Config item as identifier is not suported yet, token is {value.value}")
            else:
                #if condition is not true, it is a string literal
                if key.value in [Keyword.ENABLE_SSH.name]:
                    assert_expected_token(value, TokenType.KEYWORD)
                    expect_next_token_value([value], [Keyword.TRUE.name.upper(), Keyword.FALSE.name.upper()])
                elif key.value in [Keyword.LINE_CONSOLE.name, Keyword.SSH_KEY_SIZE.name]:
                    assert_expected_token(value, TokenType.INTEGER_LITERAL)
                else:
                    assert_expected_token(value, TokenType.STRING_LITERAL)
                    
                match key.value.upper():
                    case Keyword.HOSTNAME.name:
                        config.hostname = value.value
                    case Keyword.DOMAIN_NAME.name:
                        config.domain_name = value.value
                    case Keyword.PASSWORD.name:
                        config.password = value.value
                    case Keyword.MOTD.name:
                        config.motd = value.value
                    case Keyword.LINE_CONSOLE.name:
                        config.line_console = int(value.value)
                    case Keyword.LINE_VTY.name:
                        panic(f"parse_device_config: {key.location} unreachable")
                    case Keyword.ENABLE_SSH.name:
                        config.enable_ssh = parse_bool(value.value)
                    case Keyword.SSH_PASSWORD.name:
                        config.ssh_password = value.value
                    case Keyword.SSH_KEY_SIZE.name:
                        config.ssh_key_size = int(value.value)
                    case _:
                        panic(f"Unreachable. But somehow got here with key value {key.value}")

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {config_token.name}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)

    return config, tokens, variables 


def parseInetAddress(str: str, location: Location) -> InetAddress:
    if is_ipv4_with_mask(str):
        ip, mask, *rest = str.split()
        return InetAddress4(ip, mask)
    elif is_ipv6_with_mask(str):
        parts = str.split('/')
        return InetAddress6(parts[0], parts[1])
    panic(f"{location} Invalid IP value: {str} its not a valid IPv4 or IPv6 address")
     

def parse_vlan_info(tokens: list[Token], variables: dict[str,int|str|list|dict]) -> tuple[VLAN_INFO, list[Token], dict[str,int|str|list|dict]] :
    vlan_info = VLAN_INFO(None, None, None, None, None)
    found_items : set = set()
    vlan_expected_item_names = [
        Keyword.NAME.name,
        Keyword.NUMBER.name,
        Keyword.NETWORK_ADDRESS.name,
        Keyword.GATEWAY_ADDRESS.name,
        Keyword.DHCP.name,
    ]

    #strinping the header VLAN_INFO : {
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [Keyword.VLAN_INFO.name])
    vlan_token, *tokens = tokens

    expect_next_token_type(tokens, [TokenType.COLON])
    _ , *tokens = tokens
   
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly , *tokens = tokens

    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {vlan_token.value}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        #all vlan_attrib are simple tokens
        expect_next_token_type(tokens, [TokenType.KEYWORD])
        key, value, tokens = next_simple_pair(tokens)
        check_property(vlan_token, key, vlan_expected_item_names, found_items)

        if value.type == TokenType.IDENTIFIER:
            #TODO: Implement support for identifier names
            panic(f"{value.location} vlan item as identifier is not suported yet, token is {value.value}")
        else:
            match(key.value):
                case Keyword.NAME.name:
                    vlan_info.name = value.value
                case Keyword.NUMBER.name:
                    #TODO: add a check to see if the number is the vlan range
                    vlan_info.number = int(value.value) 
                case Keyword.NETWORK_ADDRESS.name:
                    vlan_info.network_address = parseInetAddress(value.value, value.location)
                case Keyword.GATEWAY_ADDRESS.name:
                    vlan_info.gateway_address = parseInetAddress(value.value, value.location)
                case Keyword.DHCP.name:
                    vlan_info.has_dhcp = parse_bool(value.value)
                case _:
                    panic("parse_vlans: Unreachable")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)

    return vlan_info, tokens, variables  


def parse_dict(tokens: list[Token], variables: dict[str,int|str|list|dict]) -> tuple[dict[str,int|str|list|dict], list[Token], dict[str,int|str|list|dict]]:
    res = dict()
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly_token, *tokens = tokens

    if len(tokens) == 0: panic(f"{open_curly_token.location} Unclosed {"{"}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        expect_next_token_type(tokens, [TokenType.KEYWORD, TokenType.IDENTIFIER])
        key, *tokens = tokens
        expect_next_token_type(tokens, [TokenType.COLON])
        _, *tokens = tokens

        current_token = tokens[0]
        match current_token.type:
            case TokenType.STRING_LITERAL | TokenType.INTEGER_LITERAL :
                res[key.value] = current_token.value
                _, *tokens = tokens
                tokens = remove_next_comma(tokens)
            case TokenType.OPEN_CURLY:
                res_dict, tokens, variables = parse_dict(tokens, variables)
                res[key.value] = res_dict
            case TokenType.OPEN_SQUARE:
                res_list, tokens, variables = parse_list(tokens, variables)
                res[key.value] = res_list
            case TokenType.KEYWORD:
                expect_next_token_value(tokens, [Keyword.TRUE.name, Keyword.FALSE.name])
                res[key.value] = current_token.value
                _, *tokens = tokens
                tokens = remove_next_comma(tokens)
            case _:
                panic(f"{current_token.location} Unsupported dictionary value {current_token.value}")
        if len(tokens) == 0: panic(f"{open_curly_token.location} Unclosed {"{"}")
        
    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)
    return res, tokens, variables
    

def parse_list(tokens: list[Token], variables: dict[str,int|str|list|dict]) -> tuple[list[int|str|list|dict], list[Token], dict[str,int|str|list|dict]]:
    res = []
    expect_next_token_type(tokens, [TokenType.OPEN_SQUARE])
    open_square_token, *tokens = tokens

    if len(tokens) == 0: panic(f"{open_square_token.location} Unclosed [")
    while tokens[0].type != TokenType.CLOSE_SQUARE:
        #parse content
        current_token = tokens[0]
        match current_token.type:
            case TokenType.STRING_LITERAL | TokenType.STRING_LITERAL :
                res.append(current_token.value)
                _, *tokens = tokens
                tokens = remove_next_comma(tokens)
            case TokenType.OPEN_CURLY:
                res_dict, tokens , variables = parse_dict(tokens, variables)
                res.append(res_dict)
            case TokenType.OPEN_SQUARE:
                res_list, tokens, variables = parse_list(tokens, variables)
                res.append(res_list)
            case TokenType.KEYWORD:
                expect_next_token_value(tokens, [Keyword.TRUE.name, Keyword.FALSE.name])
                res.append(current_token.value)
                _, *tokens = tokens
                tokens = remove_next_comma(tokens)
            case _:
                panic(f"{current_token.location}Unsupported list value {current_token.value}")
        if len(tokens) == 0: panic(f"{open_square_token.location} Unclosed ]")

    expect_next_token_type(tokens, [TokenType.CLOSE_SQUARE])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)
    return res, tokens, variables

def parse_switchport_access_from_dict(items: dict[int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> SWITCHPORT_ACCESS_INFO:
    switch_port_info = SWITCHPORT_ACCESS_INFO(None, None)
    found_items: set[str] = set()

    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location} Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.IF_NAME.name:
                switch_port_info.name = item_value
            case Keyword.VLAN_NUMBER.name:
                if not vlan_infos.__contains__(int(item_value)):
                     panic(f"{location} VLAN {item_value} is not defined in the configuration file")
                switch_port_info.vlan = vlan_infos[int(item_value)]
            case Keyword.DESCRIPTION.name:
                switch_port_info.description = item_value
            case Keyword.SHUTDOWN.name:
                switch_port_info.shutdown = parse_bool(item_value)
            case _:
                panic(f"{location} Unexpected ACCESS item: {item_value}")

    return switch_port_info

def parse_switchport_trunk_from_dict(items: dict[int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> SWITCHPORT_ACCESS_INFO:
    switch_port_info = SWITCHPORT_TRUNCK_INFO(None)
    found_items: set[str] = set()

    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location} Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.IF_NAME.name:
                switch_port_info.name = item_value
            case Keyword.DESCRIPTION.name:
                switch_port_info.description = item_value
            case Keyword.SHUTDOWN.name:
                switch_port_info.shutdown = parse_bool(item_value)
            case _:
                panic(f"{location} Unexpected TRUNK item: {item_value}")

    return switch_port_info

def parse_switchport_vlan_from_dict(items: dict[int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> SWITCHPORT_ACCESS_INFO:
    switch_port_info = INTERFACE_VLAN_INFO(None)
    found_items: set[str] = set()
    interface_ip: InetAddress = None 
    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location} Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.SWITCH_IP.name:
                interface_ip = parseInetAddress(item_value, location)
            case Keyword.VLAN_NUMBER.name:
                switch_port_info.vlan = vlan_infos[int(item_value)]
            case Keyword.DESCRIPTION.name:
                switch_port_info.description = item_value
            case Keyword.SHUTDOWN.name:
                switch_port_info.shutdown = parse_bool(item_value)
            case _:
                panic(f"{location} Unexpected VLAN item: {item_name}")
        
    if interface_ip is None: panic(f"{location} SWITCH_IP was not specified for INTERFACE VLAN")
    if switch_port_info.vlan is None: panic(f"{location} VLAN_NUMBER was not specified for INTERFACE VLAN")
        
    switch_port_info.vlan.network_address = interface_ip
       
    return switch_port_info
        

def parse_router_interface_from_dict(items: dict[str, int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> ROUTER_INTERFACE_INFO:
    router_interface_info = ROUTER_INTERFACE_INFO(None, None, "", True, None)
    found_items: set[str] = set()

    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location} Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.IF_NAME.name:
                router_interface_info.name = item_value
            case Keyword.VLAN_NUMBER.name:
                router_interface_info.vlan = vlan_infos[int(item_value)]
            case Keyword.DESCRIPTION.name:
                router_interface_info.description = item_value
            case Keyword.SHUTDOWN.name:
                router_interface_info.shutdown = parse_bool(item_value)
            case Keyword.CLOCK_RATE.name:
                if item_value != '': router_interface_info.clockrate = None
                router_interface_info.clockrate = int(item_value)
            case _:
                panic(f"{location} Unexpected ROUTER INTERFACE item: {item_value}")

    return router_interface_info

def parse_router_subinterface_from_dict(items: dict[str, int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> ROUTER_SUBINTERFACE_INFO:
    router_subinterface_info = ROUTER_SUBINTERFACE_INFO(None, None, "", "dot1Q", False)
    found_items: set[str] = set()

    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location} Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.IF_NAME.name:
                router_subinterface_info.name = item_value
            case Keyword.VLAN_NUMBER.name:
                router_subinterface_info.vlan = vlan_infos[int(item_value)]
            case Keyword.DESCRIPTION.name:
                router_subinterface_info.description = item_value
            case Keyword.SHUTDOWN.name:
                router_subinterface_info.shutdown = parse_bool(item_value)
            case Keyword.ENCAPSULATION.name:
                router_subinterface_info.encapsulation = item_value
            case _:
                panic(f"{location} Unexpected ROUTER INTERFACE item: {item_value}")

    return router_subinterface_info

def parse_static_routes_from_dict(items: dict[str, int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> STATIC_ROUTE:
    static_route = STATIC_ROUTE(None, None)
    found_items: set[str] = set()

    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location}: Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.NETWORK_ADDRESS.name:
                static_route.network =  parseInetAddress(item_value, location)
            case Keyword.HOP.name:
                if is_ipv4(item_value): item_value += " 0.0.0.0" #hop must be provided without mask 
                elif is_ipv6(item_value): item_value += "/0"
                static_route.hop = parseInetAddress(item_value, location) 
            case _:
                panic(f"{location} Unexpected STATIC ROUTE item: {item_value}")
    if static_route.hop is None: panic(f"{location} missing field HOP in STATIC ROUTE definition")
    if static_route.network is None: panic(f"{location} missing field NETWORK_ADDRESS in STATIC ROUTE definition")

    return static_route
    
def parse_rip_route_from_dict(items: dict[str, int|str|list|dict], vlan_infos: list[VLAN_INFO], location: Location) -> RIP_ROUTE:
    rip_route = RIP_ROUTE(2, [], [], False)
    found_items: set[str] = set()

    for item_name, item_value in items.items():
        if item_name in found_items: panic(f"{location}: Redefinition of property {item_name}")
        found_items.add(item_name)
        match(item_name):
            case Keyword.NETWORKS.VERSION.name:
                rip_route.version = int(item_value)
            case Keyword.NETWORKS.name:
                if not isinstance(item_value, list): panic(f"{location} Expected networks to be a list.")
                networks: list[InetAddress] = []
                for s in item_value:
                    if not (is_ipv4(s) or is_ipv6(s)): panic(f"{location} items contains {s} which is not a valid IP string")
                    if is_ipv4(s): s += " 0.0.0.0" #hop must be provided without mask 
                    elif is_ipv6(s): s += "/0"
                    networks.append(parseInetAddress(s, location))
                rip_route.networks = networks
            case Keyword.PASSIVE_INTERFACES.name:
                if not isinstance(item_value, list): panic(f"{location} Expected networks to be a list.")
                rip_route.passive_interfaces = item_value
            case Keyword.DEFAULT_INFORMATION.name:
                rip_route.is_default_information = parse_bool(item_value)
            case _:
                panic(f"{location} Unexpected RIP ROUTE item: {item_name}")
    return rip_route


def parse_interfaces(tokens: list[Token], variables: dict[str,int|str|list|dict], vlan_infos: list[VLAN_INFO]) -> tuple[INTERFACE_INFO, list[Token], dict[str,int|str|list|dict]]:
    #TODO: make so that one of the parameters is interfaces_expected_items
    interfaces: list[INTERFACE_INFO] = []
    found_items : set = set()
    interfaces_expected_item_names = [
        Keyword.ACCESS.name,  
        Keyword.TRUNK.name,  
        Keyword.VLAN.name, 
        Keyword.INTERFACE.name,
        Keyword.SUBINTERFACE.name, 
    ]

    #strinping the header INTERFACES:{
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [Keyword.INTERFACES.name])
    interfaces_token, *tokens = tokens

    expect_next_token_type(tokens, [TokenType.COLON])
    _ , *tokens = tokens
   
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly , *tokens = tokens

    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {interfaces_token.value}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        
        if len(tokens) < 3: panic(f"{tokens[0].location} incomplete specification of switch item")
        expect_next_token_type(tokens, [TokenType.KEYWORD])
        check_property(interfaces_token, tokens[0], interfaces_expected_item_names, found_items)
        expect_next_token_type(tokens[1:], [TokenType.COLON])
        next_value_token = tokens[2]
        key = tokens[0]
       
        if next_value_token.type == TokenType.IDENTIFIER:
            _, _, _, *tokens = tokens
            interface_type_name = key.value
            variable_name = next_value_token.value
            variable_value = variables[variable_name]

            if not isinstance(variable_value, list): panic(f"{key.location} Expected variable to be a list")    
            match(interface_type_name):
                case Keyword.ACCESS.name:
                    for dct in variable_value:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_switchport_access_from_dict(dct, vlan_infos, key.location))
                case Keyword.TRUNK.name:
                    for dct in variable_value:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_switchport_trunk_from_dict(dct, vlan_infos, key.location))
                case Keyword.VLAN.name:
                    for dct in variable_value:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_switchport_vlan_from_dict(dct, vlan_infos, key.location))
                case Keyword.INTERFACE.name:
                    for dct in variable_value:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_router_interface_from_dict(dct, vlan_infos, key.location))   
                case Keyword.INTERFACE.name:
                    for dct in variable_value:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_router_subinterface_from_dict(dct, vlan_infos, key.location))         
                case _:     
                    panic("parse_interfaces: unreachable") 
            tokens = remove_next_comma(tokens)         
        else: 
            expect_next_token_type(tokens[2:], [TokenType.OPEN_SQUARE])
            _, _, *tokens = tokens
            match(key.value):
                case Keyword.ACCESS.name:
                    item_list, tokens, variables = parse_list(tokens, variables)
                    for dct in item_list:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_switchport_access_from_dict(dct, vlan_infos, key.location))
                case Keyword.TRUNK.name:
                    item_list, tokens, variables = parse_list(tokens, variables)
                    for dct in item_list:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_switchport_trunk_from_dict(dct, vlan_infos, key.location))
                case Keyword.VLAN.name:
                    item_list, tokens, variables = parse_list(tokens, variables)
                    for dct in item_list:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_switchport_vlan_from_dict(dct, vlan_infos, key.location))
                case Keyword.INTERFACE.name:
                    item_list, tokens, variables = parse_list(tokens, variables)
                    for dct in item_list:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_router_interface_from_dict(dct, vlan_infos, key.location))
                case Keyword.SUBINTERFACE.name:
                    item_list, tokens, variables = parse_list(tokens, variables)
                    for dct in item_list:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        interfaces.append(parse_router_subinterface_from_dict(dct, vlan_infos, key.location))
                case _:     
                    panic("parse_interfaces: unreachable")

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {interfaces_token.value}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)
    
    return interfaces, tokens, variables
    

def parse_routes(tokens: list[Token], variables: dict[str, int|str|list|dict], vlan_infos: dict[int, VLAN_INFO])-> tuple[list[ROUTE], list[Token], dict[str,int|str|list|dict]]:
    routes: list[ROUTE] = []
    found_items : set = set()
    routes_expected_item_names = [
       Keyword.STATIC.name,
       Keyword.RIP.name,
    ]

    #strinping the header ROUTES:{
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [Keyword.ROUTES.name])
    routes_token, *tokens = tokens

    expect_next_token_type(tokens, [TokenType.COLON])
    _ , *tokens = tokens
   
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly , *tokens = tokens

    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {routes_token.value}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        if len(tokens) < 3: panic(f"{tokens[0].location} incomplete specification of routes item")
        expect_next_token_type(tokens, [TokenType.KEYWORD])
        check_property(routes_token, tokens[0], routes_expected_item_names, found_items)
        expect_next_token_type(tokens[1:], [TokenType.COLON])
        next_value_token = tokens[2]
        key = tokens[0]
        if next_value_token.type == TokenType.IDENTIFIER:
            _, _, _, *tokens = tokens
            interface_type_name = key.value
            variable_name = next_value_token.value
            variable_value = variables[variable_name]

            if not isinstance(variable_value, list): panic(f"{key.location} Expected variable to be a list")    
            match(interface_type_name):
                case Keyword.STATIC.name:
                     for dct in variable_value:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        routes.append(parse_static_routes_from_dict(dct, vlan_infos, key.location))
            tokens = remove_next_comma(tokens)  
        else:  
            expect_next_token_type(tokens[2:], [TokenType.OPEN_SQUARE, TokenType.OPEN_CURLY])
            _, _, *tokens = tokens
            match(key.value):
                case Keyword.STATIC.name:
                    item_list, tokens, variables = parse_list(tokens, variables)
                    for dct in item_list:
                        if not isinstance(dct, dict): panic(f"{key.location} Expected variable list items to be dictionaries")
                        routes.append(parse_static_routes_from_dict(dct, vlan_infos, key.location))
                case Keyword.RIP.name:
                    rip_dct, tokens, variables = parse_dict(tokens, variables)
                    routes.append(parse_rip_route_from_dict(rip_dct, vlan_infos, key.location))
                case _:
                    panic("parse_interfaces: Unreachable location")

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {routes_token.value}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)

    return routes, tokens, variables

    
def parse_switch_info(tokens: list[Token], variables: dict[str,int|str|list|dict], vlan_infos: list[VLAN_INFO]) -> tuple[list[Token], dict[str,int|str|list|dict], dict[str,int|str|list|dict]]:
    switch_name: str = None,
    switch_config: CONFIG_INFO = CONFIG_INFO()
    switch_iterfaces: list[INTERFACE_INFO] = []
    routes: list[ROUTE] = []
    found_items: set[str] = set()
    switch_expected_item_names = [
        Keyword.NAME.name,
        Keyword.CONFIG.name,
        Keyword.INTERFACES.name,
    ]

    #strinping the header SWITCH_INFO : {
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [Keyword.SWITCH_INFO.name])
    switch_token, *tokens = tokens

    expect_next_token_type(tokens, [TokenType.COLON])
    _ , *tokens = tokens
   
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly , *tokens = tokens

    #parse the body of SWITCH
    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {switch_token.value}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        if len(tokens) < 3: panic(f"{tokens[0].location} incomplete specification of switch item")
        expect_next_token_type(tokens, [TokenType.KEYWORD])
        check_property(switch_token, tokens[0], switch_expected_item_names, found_items)
        expect_next_token_type(tokens[1:], [TokenType.COLON])
        next_value_token = tokens[2]
        key = tokens[0]
        if next_value_token.type == TokenType.IDENTIFIER:
            #TODO: Implement support for identifier names
            panic(f"{next_value_token.location} vlan item as identifier is not suported yet, token is {next_value_token.value}")
        else:  
            match(key.value):
                case Keyword.NAME.name:
                    expect_next_token_type(tokens[2:], [TokenType.STRING_LITERAL])
                    _, _, name_token, *tokens = tokens  # first two are item_name and colon (:)
                    switch_name = name_token.value
                    tokens = remove_next_comma(tokens)  
                case Keyword.CONFIG.name:
                    switch_config, tokens, variables = parse_device_config(tokens, variables)
                case Keyword.INTERFACES.name:
                    switch_iterfaces, tokens, variables = parse_interfaces(tokens, variables, vlan_infos)
                case Keyword.ROUTES.name:
                    routes, tokens, variables = parse_routes(tokens, variables, vlan_infos)
                case _:
                    panic("parse_interfaces: Unreachable location")
                    pass

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {switch_token.value}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)

    #TODO: add parsing routes (for allowing LAYER 3 SWITCHES)
    switch_info = SWITCH_INFO(switch_name, switch_config, switch_iterfaces, routes)
    return switch_info, tokens, variables
    

def parse_router_info(tokens: list[Token], variables: dict[str, int|str|list|dict], vlan_infos: dict[int, VLAN_INFO] ) -> tuple[ROUTER_INFO, list[Token], dict[str, int|str|list|dict]]:
        router_name: str = None,
        router_config: CONFIG_INFO = CONFIG_INFO()
        router_iterfaces: list[INTERFACE_INFO] = []
        found_items: set[str] = set()
        routes: list[ROUTE] = []
        router_expected_item_names = [
            Keyword.NAME.name,
            Keyword.CONFIG.name,
            Keyword.INTERFACES.name,
            Keyword.ROUTES.name,
        ]

        #strinping the header SWITCH_INFO : {
        expect_next_token_type(tokens, [TokenType.KEYWORD])
        expect_next_token_value(tokens, [Keyword.ROUTER_INFO.name])
        router_token, *tokens = tokens

        expect_next_token_type(tokens, [TokenType.COLON])
        _ , *tokens = tokens
    
        expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
        open_curly , *tokens = tokens

        #parse the body of SWITCH
        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {router_token.value}")
        while tokens[0].type != TokenType.CLOSE_CURLY:
            if len(tokens) < 3: panic(f"{tokens[0].location} incomplete specification of router item")
            expect_next_token_type(tokens, [TokenType.KEYWORD])
            check_property(router_token, tokens[0], router_expected_item_names, found_items)
            expect_next_token_type(tokens[1:], [TokenType.COLON])
            next_value_token = tokens[2]
            key = tokens[0]
            if next_value_token.type == TokenType.IDENTIFIER:
                panic(f"{next_value_token.location} vlan item as identifier is not suported yet, token is {next_value_token.value}")
            else:  
                match(key.value):
                    case Keyword.NAME.name:
                        expect_next_token_type(tokens[2:], [TokenType.STRING_LITERAL])
                        _, _, name_token, *tokens = tokens  # first two are item_name and colon (:)
                        router_name = name_token.value
                        tokens = remove_next_comma(tokens)  
                    case Keyword.CONFIG.name:
                        router_config, tokens, variables = parse_device_config(tokens, variables)
                    case Keyword.INTERFACES.name:
                        router_iterfaces, tokens, variables = parse_interfaces(tokens, variables, vlan_infos)
                    case Keyword.ROUTES.name:
                        routes, tokens, variables = parse_routes(tokens, variables, vlan_infos)
                    case _:
                        panic("parse_interfaces: Unreachable location")
                        pass

            if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {router_token.value}")

        expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
        _, *tokens = tokens
        tokens = remove_next_comma(tokens)

        router_info = ROUTER_INFO(router_name, router_config, router_iterfaces, routes)
        return router_info, tokens, variables


def parse_config(tokens: list[Token]) -> list[DEVICE_INFO]:                 # for switches and routers
    #for defered information we have: item, variable
    # where variable has the expected value for item
    # values are quoted, items and variables are not               
    #items
    variables: dict = dict()
    vlan_infos: dict[int, VLAN_INFO] = dict()   
    device_infos: list[DEVICE_INFO] = []
    variables: dict[str,int|str|list|dict] = dict()

    default_configuration: CONFIG_INFO = None    #will be filled with information comming for default configuration if defined in the config file
    #flags:
    is_default_configuration_parsed: bool = False

    #First pass:
    while len(tokens) > 0:
        first_token: Token = tokens[0]
        match(first_token.type):
            case TokenType.KEYWORD:
                match(first_token.value):
                    case Keyword.DEFAULT_CONFIG.name:
                        if is_default_configuration_parsed: panic(f"{first_token.location} Redefinition of DEFAULT_CONFIG")
                        default_configuration, tokens, variables = parse_device_config(tokens, variables)
                        is_default_configuration_parsed = True
                    case Keyword.VLAN_INFO.name:
                        vlan, tokens, variables = parse_vlan_info(tokens, variables)
                        vlan_infos[vlan.number] = vlan
                    case Keyword.SWITCH_INFO.name:
                        switch_info, tokens, variables = parse_switch_info(tokens, variables, vlan_infos)
                        device_infos.append(switch_info)
                    case Keyword.ROUTER_INFO.name:
                        router_info, tokens, variables = parse_router_info(tokens, variables, vlan_infos)
                        device_infos.append(router_info)
                    case _:   
                        #debug exit
                        # print("*"*40)
                        # print(f"default_config: {default_configuration}")
                        # print("*"*40)
                        # print(f"Parsed vlans:\n{vlan_infos}")
                        # print("*"*40)
                        # print(f"Parsed devices:\n{device_infos}")
                        # print("*"*40)
                        # print(f"Parsed variables{variables}")
                        # print("*"*40)
                        panic(f"{first_token.location} {first_token.value} keyword is not expected at the top level")
            case TokenType.IDENTIFIER:
                indetifier_token, *tokens = tokens
                expect_next_token_type(tokens, [TokenType.COLON])
                _, *tokens = tokens
                expect_next_token_type(tokens, [TokenType.OPEN_CURLY, TokenType.OPEN_SQUARE])
                opening_token = tokens[0]

                match opening_token.type:
                    case TokenType.OPEN_CURLY:
                       content, tokens, variables = parse_dict(tokens, variables) 
                       variables[indetifier_token.value] = content
                    case TokenType.OPEN_SQUARE:
                        content, tokens, variables = parse_list(tokens, variables)
                        variables[indetifier_token.value] = content
                    case _:
                        panic(f"parse_config: Unreachable")
            case _ :
                #debug exit
                # print("*"*40)
                # print(f"default_config: {default_configuration}")
                # print("*"*40)
                # print(f"Parsed vlans:\n{vlan_infos}")
                # print("*"*40)
                # print(f"Parsed devices:\n{device_infos}")
                # print("*"*40)
                # print(f"Parsed constants:\n{variables}")
                # print("*"*40)
                panic(f"{tokens[0].location} Expected a keyword or identifier but found {tokens[0].type} is not implemented yet")

    #Second Pass. 
    #Unspecified Device config values, that were initialized with None in the first pass (Note: not the 'None' string), will be filled with information of device config
    for device_info in device_infos:
        if device_info.config_info is None: device_info.config_info = CONFIG_INFO() #If somehow config IF was not specified in the configuration file
        if device_info.config_info.domain_name is None: device_info.config_info.domain_name = default_configuration.domain_name
        if device_info.config_info.enable_ssh is None: device_info.config_info.enable_ssh = default_configuration.enable_ssh
        if device_info.config_info.hostname is None: device_info.config_info.hostname = default_configuration.hostname
        if device_info.config_info.line_console is None: device_info.config_info.line_console = default_configuration.line_console
        if device_info.config_info.line_vty is None: device_info.config_info.line_vty= default_configuration.line_vty
        if device_info.config_info.motd is None: device_info.config_info.motd = default_configuration.motd
        if device_info.config_info.password is None: device_info.config_info.password = default_configuration.password
        if device_info.config_info.ssh_key_size is None: device_info.config_info.ssh_key_size = default_configuration.ssh_key_size
        if device_info.config_info.ssh_password is None: device_info.config_info.ssh_password = default_configuration.ssh_password

    return device_infos

def parse_config_file(config_file_path: str) -> list[DEVICE_INFO]:
    with open(config_file_path) as f:
        tokens = lex_config_from_file(config_file_path)
        if len(tokens) == 0: print("Empty configuration input file")
        devices = parse_config(tokens)
    return devices
