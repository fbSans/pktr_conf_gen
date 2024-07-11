from dataclasses import dataclass
from enum import Enum


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


class Keyword(Enum):
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
    NETWORK_ADDRESS = iota()
    GATEWAY_ADDRESS = iota()
    DHCP = iota()

    #interface type specific information:
    ACCESS = iota()
    TRUNK = iota()
    VLAN = iota()

    #device (switch or router) specific information
    IF_NAME = iota()
    VLAN_NUMBER = iota()
    SWITCH_IP = iota()
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