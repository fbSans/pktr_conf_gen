import dataclasses

class InetAddress:
    def __init__(self, ip: str, mask: str):
        self.ip = ip
        self.mask: str = mask

class InetAddress4(InetAddress):
    def __init__(self, ipv4: str, mask: str):
        #add a check to ipv4
        self.__init__(ipv4, mask)
    
class InetAddress6(InetAddress):
    def __init__(self, ipv6: str, mask: str):
        #add a check to ipv6
        self.__init__(ipv6, mask)

@dataclasses
class VLAN_INFO:
    name : str
    address : InetAddress
    gateway_address : InetAddress
    has_dhcp : bool 

#representa informacao de uma interface ou subinterface
class INTERFACE_INFO:
    def __init__(self, name : str, description : str = "", shutdown : bool = True):
        self.name = name
        self.description = description
        self.shutdown = shutdown

class SWITCHPORT_ACCESS_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "" , shutdown : bool = False):
        super().__init__(name, description, shutdown)
        self.vlan = vlan

class SWITCHPORT_TRUNCK_INFO(INTERFACE_INFO):
    def __init__(self, name: str, description, shutdown: bool = False):
        super().__init__(name, description, shutdown)

class ROUTER_INTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "", shutdown=True):
        super().__init__(name, description, shutdown)
    pass

class ROUTER_SUBINTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "", encapsulation: str = 'dot1Q', shutdown : bool=True):
        assert encapsulation == 'dot1Q', f"Unsupported encapsulation {encapsulation}"
        super().__init__(name, vlan, description, shutdown)

@dataclasses
class CONFIG_INFO:
    hostname : str = ''
    enable_secret : str = ''
    motd : str = ''
    line_console : list[int] = [0]
    line_vty : tuple[int,int] = (1, 4)
    enable_ssh : bool = False
    ssh_key_bit_size : int = 1024
    ssh_password : str = ''

class DEVICE_INFO:
    def __init__(self, config_info: CONFIG_INFO):
        self.config_info = config_info

class SWITCH_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, switchports: list[SWITCHPORT_ACCESS_INFO | SWITCHPORT_TRUNCK_INFO]):
        super().__init__(config_info)
        self.name = name
        self.interfaces = switchports
        
class ROUTER_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[ROUTER_INTERFACE_INFO | ROUTER_SUBINTERFACE_INFO]):
        super().__init__(config_info)
        self.name = name
        self.interfaces = interfaces


