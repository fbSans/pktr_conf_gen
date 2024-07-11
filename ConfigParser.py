# TODO not implemented 
from PTConfigDataStrucures import *
from ConfigToken import *
from ConfigLexer import *


#################################### Helper Functions ################################################################

#used to make assertions uppon token types and/or values. if the provided argument to the parameter value is None, value won't be checked
def assert_expected_token(actual_token: Token, expected_type: TokenType, expected_value: str = None):
    if expected_value == None:
        if not (actual_token.type == expected_type): panic(f"{actual_token.location} Expected token type {expected_type}, but found token type {actual_token.type.name}.")
    else:
        if not (actual_token.type == expected_type and actual_token.value == expected_value): 
            panic(f"{actual_token.location} Expected token type {expected_type} and value to be {expected_value}, but found token type{actual_token.type.name} and value {actual_token.value}.")


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
        panic(f"expected to find token type in {expected_token_values}, but found {tokens[0].value}")


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
    config: CONFIG_INFO = CONFIG_INFO()
    config_expected_item_names = [
                        Keyword.HOSTNAME.name, 
                        Keyword.DOMAIN_NAME.name, 
                        Keyword.PASSWORD.name, 
                        Keyword.MOTD.name, 
                        Keyword.LINE_CONSOLE.name, 
                        Keyword.LINE_VTY.name, 
                        Keyword.ENABLE_SSH.name , 
                        Keyword.SSH_PASSWORD.name
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
   
    #lack of do while loop ;)
    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {config_token.name}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        #line vty an object
        if tokens[0].type == TokenType.KEYWORD and tokens[0].value == Keyword.LINE_VTY.name:
            start, end, tokens = parse_line_vty(tokens)
            config.line_vty = start, end
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
            elif key.value in [Keyword.LINE_CONSOLE.name]:
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
                case _:
                    panic(f"Unreachable. But somehow got here with key value {key.value}")

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {config_token.name}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)

    return config, tokens, variables 

def is_ip_v4(str: str) -> bool:
    parts = str.split('.')
    if len(parts) != 4: return False
    for part in parts:
        if not part.isdigit() and not (0 <= int(part) <= 255): return False
    return True

#takes a string of the form "xxx.xxx.xxx.xxx xxx.xxx.xxx.xxx" and returns and I
def parseInetAddress(str: str, location: Location) -> InetAddress:
    ip, mask, *rest = str.split()
    if rest != []:
        print(str)
        panic(f"{location} The ip string must contain only to parts: the ip address and the mask")

    if is_ip_v4(ip) and is_ip_v4(mask):
        return InetAddress4(ip, mask)
    
    #TODO: add support to IPV6
    panic(f"{location} Invalid IP value: {str}. Note: Only IPv4 is supported")
     

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
                expect_next_token_value[Keyword.TRUE.name, Keyword.FALSE.name]
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


def parse_interfaces(tokens: list[Token], variables: dict[str,int|str|list|dict]) -> tuple[INTERFACE_INFO, list[Token], dict[str,int|str|list|dict]]:
    interfaces: list[INTERFACE_INFO] = []
    found_items : set = set()
    interfaces_expected_item_names = [
        Keyword.ACCESS.name,  
        Keyword.TRUNK.name,  
        Keyword.VLAN.name  
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
            match(interface_type_name):
                case Keyword.ACCESS.name:
                    panic("parse_interfaces: parsing access interfaces is not implemented yet")
                case Keyword.TRUNK.name:
                    panic("parse_interfaces: parsing trunk interfaces is implemented yet")
                case Keyword.VLAN.name:
                    panic("parse_interfaces: parsing vlan interfaces is not implemented yet")
                case _:     
                    panic("parse_interfaces: unreachable")               
            panic(f"{next_value_token.location} interfaces item as identifier is not suported yet, token is {next_value_token.value}")
        else: 
            expect_next_token_type(tokens[2:], [TokenType.OPEN_SQUARE])
            match(key.value):
                case Keyword.ACCESS.name:
                    panic("parse_interfaces: parsing access interfaces is not implemented yet")
                case Keyword.TRUNK.name:
                    panic("parse_interfaces: parsing trunk interfaces is implemented yet")
                case Keyword.VLAN.name:
                    panic("parse_interfaces: parsing vlan interfaces is not implemented yet")
                case _:     
                    panic("parse_interfaces: unreachable")

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {interfaces_token.value}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)
    
    return interfaces, tokens, variables
    

    
def parse_switch_info(tokens: list[Token], variables: dict[str,int|str|list|dict]) -> tuple[list[Token], dict[str,int|str|list|dict], dict[str,int|str|list|dict]]:
    switch_name: str = None,
    switch_config: CONFIG_INFO = CONFIG_INFO()
    switch_iterfaces: list = []
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
                    switch_iterface, tokens, variables = parse_interfaces(tokens, variables)
                    switch_iterfaces.append(switch_iterface)
                case _:
                    panic("parse_interfaces: Unreachable location")
                    pass

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {switch_token.value}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    tokens = remove_next_comma(tokens)

    switch_info = SWITCH_INFO(switch_name, switch_config, switch_iterfaces)
    return switch_info, tokens, variables
    

def parse_config(tokens: list[Token]) -> list[DEVICE_INFO]:                 # for switches and routers
    #for defered information we have: item, variable
    # where variable has the expected value for item
    # values are quoted, items and variables are not               
    #intems
    variables: dict = dict()
    vlan_infos: dict[int, VLAN_INFO] = dict()   
    interfaces: list[INTERFACE_INFO] = []
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
                        switch_info, tokens, variables = parse_switch_info(tokens, variables)
                        device_infos.append(switch_info)
                    case _:   
                         #debug exit
                        print("*"*40)
                        print(f"default_config: {default_configuration}")
                        print("*"*40)
                        print(f"Parsed vlans:\n{vlan_infos}")
                        print("*"*40)
                        print(f"Parsed devices:\n{device_infos}")
                        print("*"*40)
                        print(f"Parsed variables{variables}")
                        print("*"*40)
                        panic(f"parsing {first_token.value} Not implemented yet")
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
                print("*"*40)
                print(f"default_config: {default_configuration}")
                print("*"*40)
                print(f"Parsed vlans:\n{vlan_infos}")
                print("*"*40)
                print(f"Parsed devices:\n{device_infos}")
                print("*"*40)
                print(f"Parsed constants:\n{variables}")
                print("*"*40)
                panic(f"Parsing for  {tokens[0].type} is not implemented yet")


#TODO: Generate a config from the parsed file
if __name__ == '__main__':
    tokens = lex_config_from_file("test_configuration")
    print(parse_config(tokens))
    