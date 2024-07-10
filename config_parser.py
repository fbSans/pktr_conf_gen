# TODO not implemented 
from config_data_structures import *
from enum import Enum
from dataclasses import dataclass
import sys

        
def remove_current_line(str: str) -> str:
    return  str.split('\n', 1)[1]

def next_word(str: str) -> tuple[str, str]:
    if not (str[0].isalpha() or str[0] == '_'): return ['', str]
    
    i = 0
    while(i < len(str) and (str[i].isalnum() or str[i] == '_')):  i+=1
    if(i == len(str)): return (str, "")
    return (str[:i], str[i:])

def next_integer(str: str) -> tuple[str, str]:    
    i = 0
    while(i < len(str) and str[i].isdigit()):  i+=1
    if(i == len(str)): return (str, "")
    return str[:i], str[i:]

def next_string(str: str, end: str ='"') -> tuple[str, str]:
    res = str.split(end, 1)
    return res[0], res[1]

counter = 0
def iota():
    global counter 
    counter += 1
    return counter
def reset():
    global counter
    counter = 0

class TokenType(Enum):
    reset()
    IDENTIFIER = iota()
    KEYWORD = iota()
    STRING_LITERAL = iota()
    INTEGER_LITERAL = iota()
    COMMA = iota()
    DOT = iota()
    COLON = iota()
    OPEN_PAREN = iota()
    CLOSE_PAREN = iota()
    OPEN_CURLY = iota()
    CLOSE_CURLY = iota()
    OPEN_SQUARE = iota()
    CLOSE_SQUARE = iota()
    UNKNOWN = iota()
    reset()

string_to_tokentype_dict = {
    '{' : TokenType.OPEN_CURLY, 
    '}' : TokenType.CLOSE_CURLY, 
    ',' : TokenType.COMMA, 
    ':' : TokenType.COLON, 
    '[' : TokenType.OPEN_SQUARE, 
    ']' : TokenType.CLOSE_SQUARE,  
    '.' : TokenType.DOT,
}

# resolves token type for special words. Does not resolve Identifiers or StringLiteral
def token_type_from_string(str: str):
    str = str.upper()
    if str in string_to_tokentype_dict:
        return string_to_tokentype_dict[str]
    if str in KeyWord.__members__:
        return TokenType.KEYWORD
    if str.isdigit():
        return TokenType.INTEGER_LITERAL
    return None  

class KeyWord(Enum):
    reset()
    #boolean values
    TRUE = iota() 
    FALSE = iota()

    #info types
    VLAN_INFO = iota()   
    SWITCH_INFO = iota()
    ROUTER_INFO = iota()
    CONFIG = iota()

    #config_info
    DEFAULT_CONFIG = iota()
    HOSTNAME = iota()
    DOMAIN_NAME = iota()
    PASSWORD  = iota()
    MOTD = iota()
    LINE_CONSOLE  = iota()
    LINE_VTY = iota()
    START = iota()
    END = iota()
    ENABLE_SSH  = iota()
    SSH_PASSWORD = iota()

    #vlan specific information
    NAME = iota()
    NUMBER = iota()
    IP_ADDRESS = iota()
    GATEWAY = iota()
    DHCP = iota()

    #device (switch or router) specific information
    IF_NAME = iota()
    VLAN_NUMBER = iota()
    DESCRIPTION = iota()
    SHUTDOWN = iota()
    INTERFACE = iota()
    INTERFACES = iota()
    reset()


@dataclass
class Location:
    filepath : str
    line: int
    column: int

    def __str__(self):
        return self.filepath + ':' + str(self.line) + ':' + str(self.column) 
    
class Token:
    def __init__(self, type: TokenType, value : str, location: Location):
        self.type = type
        self.value = value
        self.location = location
    
    def __str__(self) -> str:
        return f"Token(type: {self.type.name},  value: '{self.value}',  location: {self.location})"
    
    def __repr__(self) -> str:
        return self.__str__()
    
def panic(msg: str):
    print(f"ERROR: {msg}", file=sys.stderr)
    exit(1)


#Invariat: Whole tokens only remain in one line, they can't be split accross lines
#This function is now responsible for making line and col count for tokens
def lex_config_from_string(config_string: str, row: int, filepath: str) -> list[Token]:
    tokens: list[Token] = []
    original_size = len(config_string)
    config_string = config_string.lstrip()
    while(len(config_string) > 0):
        token_type: TokenType = TokenType.UNKNOWN 
        token_value: str = '' 
        #print(bag)
        match config_string[0] :
            case '#':
                config_string = remove_current_line(config_string)
            case '{' | '}' | ',' | ':' | '[' | ']' | '.'  as c:
                token_value = c
                token_type = token_type_from_string(c)
                config_string = config_string[1:]
            case '"' | "'" as q:
                token_type = TokenType.STRING_LITERAL
                #TODO: make sure that the string literal close
                string_literal, config_string = next_string(config_string[1:], q)
                token_value = string_literal
            case _ :
                if config_string[0].isdigit():
                    number, config_string = next_integer(config_string)
                    token_value = number
                    token_type = TokenType.INTEGER_LITERAL
                else:
                    assert config_string[0].isalpha() or config_string[0] == '_', print(f"unexpected character ", config_string[0] in config_string, file=sys.stderr)
                    word, config_string = next_word(config_string)
                    token_type = token_type_from_string(word)
                    token_value = word
                    if token_type is None: token_type = TokenType.IDENTIFIER 
        if(token_type != TokenType.UNKNOWN):
            col = original_size - len(config_string) - len(token_value) + 1
            token = Token(token_type, token_value, Location(filepath, row, col))
            tokens.append(token)
        config_string = config_string.lstrip()
    return tokens


#TODO: ADD a function to parse the coniguration file into a list of config_data structures

#opens a config file, parses its content into tokens of the type Token and returns them into a list
def lex_config_from_file(filepath: str) -> list[Token]:
    tokens: list[Token] = []
    with open(filepath) as f:
        row = 1
        for line in f.readlines():
            tokens.extend(lex_config_from_string(line, row, filepath))
            row += 1

        #remove enclosing {} if they exist    
        if len(tokens) >= 1 and tokens[0] == '{':
            if tokens[-1] != '}':
                panic(f"Expected matching {"}"} in end of file for the {"{"} at {tokens[0].location} ")
        tokens = tokens[1:-1]

    return tokens



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

#here simple pair means that the value is a string literal or an identifier, removes the comma in the end if it exists
def next_simple_pair(tokens: list[Token]) -> tuple[Token, Token, list[Token]]:
    expect_next_token_type(tokens, [TokenType.KEYWORD, TokenType.IDENTIFIER])
    key, *tokens = tokens
    expect_next_token_type(tokens, [TokenType.COLON])
    _, *tokens = tokens
    expect_next_token_type(tokens, [TokenType.STRING_LITERAL, TokenType.IDENTIFIER, TokenType.KEYWORD, TokenType.INTEGER_LITERAL])
    if(tokens[0].type == TokenType.KEYWORD): expect_next_token_value(tokens, [KeyWord.TRUE.name, KeyWord.FALSE.name])
    value, *tokens = tokens
    
    if len(tokens) > 0:
        expect_next_token_type(tokens, [TokenType.COMMA, TokenType.CLOSE_CURLY, TokenType.CLOSE_SQUARE, TokenType.CLOSE_PAREN])
        if tokens[0].type == TokenType.COMMA: _, *tokens = tokens
    
    return key, value, tokens
    
config_items = [KeyWord.DEFAULT_CONFIG.name, KeyWord.HOSTNAME.name, KeyWord.DOMAIN_NAME.name, KeyWord.PASSWORD.name, KeyWord.MOTD.name, KeyWord.LINE_CONSOLE.name, KeyWord.LINE_VTY.name, KeyWord.ENABLE_SSH.name , KeyWord.SSH_PASSWORD.name]

#parses line_vty from the list of tokens
def parse_line_vty(tokens: list[Token]) -> tuple[int, int, list[Token]]:
    found_items: set = set()
    start, end = 0, 0
    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [KeyWord.LINE_VTY.name])
    _ , *tokens = tokens
    expect_next_token_type(tokens, [TokenType.COLON])
    _, *tokens = tokens
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    _, *tokens = tokens
    
    for _ in range(2):
        key, value, tokens = next_simple_pair(tokens)
        assert_expected_token(key, TokenType.KEYWORD)
        expect_next_token_value([key], [KeyWord.START.name, KeyWord.END.name]) 
        if key in found_items: panic(f"{key.location} Redefinition of property {key.name} while defining property {KeyWord.LINE_VTY.name}")
        match key.value:
            case KeyWord.START.name:
               start = int(value.value) 
            case KeyWord.END.name:
                end = int(value.value)
            case _:
                #make it fail on porpose to report with the default message
                assert False, "parse_line_vty: Unreachable"

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens

    if len(tokens) > 0:
        expect_next_token_type(tokens, [TokenType.COMMA, TokenType.CLOSE_CURLY, TokenType.CLOSE_SQUARE, TokenType.CLOSE_PAREN])
        if tokens[0].type == TokenType.COMMA: _, *tokens = tokens 

    return start, end, tokens


def parse_bool(str: str):
    match str.upper():
        case "TRUE":
            return True
        case "FALSE":
            return False
        case _:
            panic(f"{str} cannot be converted into boolean")

def parse_device_config(tokens: list[Token], defered_items: list) -> tuple[CONFIG_INFO, list[Token]]:
    found_items : set = set()
    config: CONFIG_INFO = CONFIG_INFO()

    expect_next_token_type(tokens, [TokenType.KEYWORD])
    expect_next_token_value(tokens, [KeyWord.DEFAULT_CONFIG.name.upper(), KeyWord.CONFIG.name.upper()])
    config_token , *tokens = tokens

    expect_next_token_type(tokens, [TokenType.COLON])
    _ , *tokens = tokens
   
    expect_next_token_type(tokens, [TokenType.OPEN_CURLY])
    open_curly , *tokens = tokens
   
    #lack of do while loop ;)
    if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {config_token.name}")
    while tokens[0].type != TokenType.CLOSE_CURLY:
        #line vty an object
        if tokens[0].type == TokenType.KEYWORD and tokens[0].value == KeyWord.LINE_VTY.name:
            start, end, tokens = parse_line_vty(tokens)
            config.line_vty = start, end
        #to parse simple values
        expect_next_token_type(tokens, [TokenType.KEYWORD])
        key, value, tokens = next_simple_pair(tokens)
    
        if not key.value in config_items:
            panic(f"{key.location} Unknown config property {key.value}")
        if key in found_items: panic(f"{key.location} Redefinition of property {key.value} for {config_token.value}")
        found_items.add(key)

        if value.type == TokenType.IDENTIFIER:
            #TODO: Implement support for identifier names
            panic(f"{value.location} Config item as identifier is not suported yet, token is {value.value}")
        else:
            #if condition is not true, it is a string literal
            if key.value in [KeyWord.ENABLE_SSH.name]:
                assert_expected_token(value, TokenType.KEYWORD)
                expect_next_token_value([value], [KeyWord.TRUE.name.upper(), KeyWord.FALSE.name.upper()])
            elif key.value in [KeyWord.LINE_CONSOLE.name]:
                assert_expected_token(value, TokenType.INTEGER_LITERAL)
            else:
                assert_expected_token(value, TokenType.STRING_LITERAL)
                
            match key.value.upper():
                case KeyWord.HOSTNAME.name:
                    config.hostname = value.value
                case KeyWord.DOMAIN_NAME.name:
                    config.domain_name = value.value
                case KeyWord.PASSWORD.name:
                    config.password = value.value
                case KeyWord.MOTD.name:
                    config.motd = value.value
                case KeyWord.LINE_CONSOLE.name:
                    config.line_console = int(value.value)
                case KeyWord.LINE_VTY.name:
                    panic(f"{key.location} unreachable")
                case KeyWord.ENABLE_SSH.name:
                    config.enable_ssh = parse_bool(value.value)
                case KeyWord.SSH_PASSWORD.name:
                    config.ssh_password = value.value
                case _:
                    panic(f"Unreachable. But somehow got here with key value {key.value}")

        if len(tokens) == 0: panic(f"{open_curly.location} Unclosed {"}"} in the definition of {config_token.name}")

    expect_next_token_type(tokens, [TokenType.CLOSE_CURLY])
    _, *tokens = tokens
    return config, tokens 
    
    

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
                    case KeyWord.DEFAULT_CONFIG.name:
                        default_configuration, tokens = parse_device_config(tokens, deferred_names)
                    case _:
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
                panic(f"Parsing for configuration item {tokens[0].type} is not implemented yet")

#TODO: Generate a config from the parsed file
if __name__ == '__main__':
    tokens = lex_config_from_file("test_configuration")
    print(parse_config(tokens))
    