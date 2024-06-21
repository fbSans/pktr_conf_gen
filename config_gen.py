import dataclasses
import sys

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
    number : int
    network_address : InetAddress
    gateway_address : InetAddress
    has_dhcp : bool 

#representa informacao de uma interface ou subinterface
class INTERFACE_INFO:
    def __init__(self, name : str, description : str = "", shutdown : bool = True):
        self.name = name
        self.description = description
        self.shutdown = shutdown

    def generate_config_if(self, file=sys.stdout):
        if(self.description != ""): config_if += f"description {self.description}\n"
        if self.shutdown: config_if += "shutdown\n"
        else:config_if += "no shutdown\n"
        config_if +="exit\n"
    

class SWITCHPORT_ACCESS_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "" , shutdown : bool = False):
        super().__init__(name, description, shutdown)
        self.vlan = vlan

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"switchport mode access\n"
        config_if += f"switchport access vlan {self.vlan.number}\n"
        super().generate_config_if(file)
        print(config_if, file=file)      
    

class SWITCHPORT_TRUNCK_INFO(INTERFACE_INFO):
    def __init__(self, name: str, description, shutdown: bool = False):
        super().__init__(name, description, shutdown)

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"switchport mode trunk\n"
        config_if += f"switchport trunk allowed vlan all\n"
        print(config_if, file=file) 
        super().generate_config_if(file)
 

class ROUTER_INTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO | None, description: str = "", shutdown=True, clockrate: int | None = 64000 ):
        super().__init__(name, description, shutdown)
        self.vlan = vlan
        self.clockrate = clockrate

    def generate_config_if(self, file=sys.stdout):
        if self.vlan is not None:
            config_if = f"interface {self.name}\n"
            config_if += f"ip address {self.vlan.gateway_address.ip} {self.vlan.gateway_address.mask}\n"
        if self.clockrate is not None : config_if += f"clockrate {self.clockrate}\n"
        print(config_if, file=file) 
        super().generate_config_if(file)

class ROUTER_SUBINTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "", encapsulation: str = 'dot1Q', shutdown : bool=True):
        assert encapsulation == 'dot1Q', f"Unsupported encapsulation {encapsulation}"
        super().__init__(name, vlan, description, shutdown)
        self.encapsulation = encapsulation

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"encapsulation {self.en}"
        config_if += f"ip address {self.vlan.gateway_address.ip} {self.vlan.gateway_address.mask}\n"
        print(config_if, file=file) 
        super().generate_config_if(file)


@dataclasses
class CONFIG_INFO:
    hostname : str = ''
    domain_name: str = ''
    password : str = ''
    motd : str = ''
    line_console : int = 0
    line_vty : tuple[int,int] = (1, 4)
    enable_ssh : bool = False
    #ssh_key_bit_size : int = 1024   
    ssh_password : str = ''

class DEVICE_INFO:
    def __init__(self, config_info: CONFIG_INFO):
        self.config_info = config_info

    def generate_basic_config(self, file=sys.stdout):
        basic_config = "enable\n"
        basic_config += f"hostname {self.config_info.hostname}\n\n"
        if self.config_info.domain_name != '':
            basic_config += f"ip domain name {self.config_info.domain_name}\n\n"
        if self.config_info.enable_secret != '': 
            basic_config += f"enable secret {self.config_info.password}\n\n"
            basic_config += f"line console {self.config_info.line_console}\n"
            basic_config += f"password {self.config_info.password}\n"
            basic_config += f"login\n"
            if not self.config_info.enable_ssh: #because all the vty line will be hooked to ssh
                basic_config += f"exit\n"
                basic_config += f"line vty {self.config_info.line_console[0]} {self.config_info.line_vty[1]}\n"
                basic_config += f"password {self.config_info.password}\n"
                basic_config += f"login\n"
                basic_config += f"exit\n\n"    
            basic_config += f"motd #{self.config_info.motd}#\n\n"
        if self.config_info.enable_ssh:
            basic_config += "crypto key generate rsa" #hardcode rsa
            basic_config += "ip ssh version 2"
            basic_config += f"line vty 0 15\n" #harcode the range
            basic_config += f"transport input ssh\n"
            basic_config += f"login local\n\n"
        print(basic_config, file=file)

    def generate_config_if(self, file = sys.stdout):
        pass

        

class SWITCH_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[SWITCHPORT_ACCESS_INFO | SWITCHPORT_TRUNCK_INFO]):
        super().__init__(config_info)
        self.name = name
        self.interfaces = interfaces

    def generate_basic_config(self, file = sys.stdout):
        super().generate_basic_config(file)
        
class ROUTER_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[ROUTER_INTERFACE_INFO | ROUTER_SUBINTERFACE_INFO]):
        super().__init__(config_info)
        self.name = name
        self.interfaces = interfaces



def generate_config(file = sys.stdout, devices: list[SWITCH_INFO | ROUTER_INFO] = []):
    if len(devices) > 0:
        #generate config

        
        pass

