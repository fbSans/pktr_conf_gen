# TODO not implemented 
from config_data_structures import *
from Token import *
from Lexer import *


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


def parse_device_config(tokens: list[Token], defered_names: dict) -> tuple[CONFIG_INFO, list[Token], dict]:
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
                    panic(f"{key.location} unreachable")
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

    return config, tokens, defered_names 
    

def parse_vlan(tokens: list[Token], deffered_names: dict) -> tuple[VLAN_INFO, list[Token], dict] :
    found_items : set = set()
    vlan_items = [
        Keyword.NAME.name,
        Keyword.NUMBER.name,
        Keyword.IP_ADDRESS.name,
        Keyword.GATEWAY.name,
        Keyword.DHCP.name,
    ]
    panic("parse_vlans: not implemented yet")  

def parse_config(tokens: list[Token]) -> list[DEVICE_INFO]:                 # for switches and routers
    #for defered information we have: item, variable
    # where variable has the expected value for item
    # values are quoted, items and variables are not               
    #intems
    deferred_names: dict = dict()
    vlan_info: dict[str, VLAN_INFO] = dict()   
    interfaces: list[INTERFACE_INFO] = []
    device_infos: list[DEVICE_INFO] = []

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
                        default_configuration, tokens, deferred_names = parse_device_config(tokens, deferred_names)
                    case Keyword.VLAN_INFO.name:
                        vlan, tokens, deferred_names = parse_vlan(tokens, deferred_names)
                    case _:   
                         #debug exit
                        print("*"*40)
                        print(f"default_config: {default_configuration}")
                        print("*"*40)
                        print(f"Parsed vlans:\n{vlan_info}")
                        print("*"*40)
                        print(f"Parsed devices:\n{device_infos}")
                        print("*"*40)
                        print(f"Parsed defered_info:\n{deferred_names}")
                        print("*"*40)
                        panic(f"parsing {first_token.value} Not implemented yet")
            case TokenType.IDENTIFIER:
                panic(f"Parsing {first_token.value} implemented yet")
            case _ :
                #debug exit
                print("*"*40)
                print(f"default_config: {default_configuration}")
                print("*"*40)
                print(f"Parsed vlans:\n{vlan_info}")
                print("*"*40)
                print(f"Parsed devices:\n{device_infos}")
                print("*"*40)
                print(f"Parsed defered_info:\n{deferred_names}")
                print("*"*40)
                panic(f"Parsing for  {tokens[0].type} is not implemented yet")

#TODO: Generate a config from the parsed file
if __name__ == '__main__':
    tokens = lex_config_from_file("test_configuration")
    print(parse_config(tokens))
    