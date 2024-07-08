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


def lex_configs(config_string: str) -> list[str]:
    bag = []
    config_string = config_string.lstrip()
    while(len(config_string) > 0):
        #print(bag)
        match config_string[0] :
            case '#':
                config_string = remove_current_line(config_string)
            case '{' | '}' | ',' | ':' | '[' | ']' | '.'  as c:
                bag.append(c)
                config_string = config_string[1:]
            case '"' | "'" as q:
                string_literal, config_string = next_string(config_string[1:], q)
                bag.append(string_literal)
            case _ :
                if config_string[0].isdigit():
                    number, config_string = next_integer(config_string)
                    bag.append(number)
                else:
                    assert config_string[0].isalpha() or config_string[0] == '_', print(f"unexpected character ", config_string[0] in config_string, file=sys.stderr)
                    word, config_string = next_word(config_string)
                    bag.append(word)
            
        config_string = config_string.lstrip()
    return bag

#TODO: ADD a function to parse the coniguration file into a list of config_data structures
#TODO: Generate a config from the parsed file

if __name__ == '__main__':
    with open("test_configuration") as f:
        config = ''
        for line in f.readlines():
            config += line
        print(lex_configs(config))
    
    #print(lex_configs("{ip: 192.168.10.1 255.255.255.128}"))