from ConfigToken import *
import sys


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
    if str in Keyword.__members__:
        return TokenType.KEYWORD
    if str.isdigit():
        return TokenType.INTEGER_LITERAL
    return None  


def remove_current_line(str: str) -> str:
    return  str.split('\n', 1)[1]


def next_word(str: str) -> tuple[str, str]:
    if not (str[0].isalpha() or str[0] == '_'): return ('', str)
    
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


