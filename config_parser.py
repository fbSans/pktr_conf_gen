# TODO not implemented 
from config_data_structures import *
from enum import Enum
from dataclasses import dataclass

        
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
    '{' : TokenType.CLOSE_CURLY, 
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
    HOSTNAME = iota()
    DOMAIN_NAME = iota()
    PASSWORD  = iota()
    MOTD = iota()
    LINE_CONSOLE  = iota()
    LINE_VTY = iota()
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

#Invariat: Whole tokens only remain in one line, they can't be split accross lines
#This function is now responsible for making line and col count for tokens
def lex_config_from_string(config_string: str, row: int, filepath: str) -> list[Token]:
    bag: list[Token] = []
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
                #TODO: make sure that the string literal close
                string_literal, config_string = next_string(config_string[1:], q)
                token_value = string_literal
                token_type = TokenType.STRING_LITERAL
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

        if token_value != '':
            col = original_size - len(config_string) - len(token_value) + 1
            token = Token(token_type, token_value, Location(filepath, row, col))
            bag.append(token)
        config_string = config_string.lstrip()
    return bag


#TODO: ADD a function to parse the coniguration file into a list of config_data structures
def lex_config_from_file(filepath: str) -> list[Token]:
    bag: list[Token] = []
    with open(filepath) as f:
        row = 1
        for line in f.readlines():
            bag.extend(lex_config_from_string(line, row, filepath))
            row += 1
    return bag


#TODO: Generate a config from the parsed file



if __name__ == '__main__':
    tokens = lex_config_from_file("test_configuration")

    for token in tokens:
        print(token)