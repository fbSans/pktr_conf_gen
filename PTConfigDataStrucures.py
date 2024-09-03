from dataclasses import dataclass
from abc import ABC, abstractmethod
import sys

class InetAddress:
    def __init__(self, ip: str, mask: str):
        self.ip = ip
        self.mask: str = mask
    def __str__(self):
        return f"InetAddress(ip:{self.ip}, mask:{self.mask})"
    
    def __repr__(self):
        return self.__str__()

class InetAddress4(InetAddress):
    def __init__(self, ipv4: str, mask: str):
        #add a check to ipv4
        super().__init__(ipv4, mask)
    
class InetAddress6(InetAddress):
    def __init__(self, ipv6: str, mask: str):
        #add a check to ipv6
        super().__init__(ipv6, mask)

@dataclass
class ROUTE(ABC):
    pass

@dataclass
class STATIC_ROUTE(ROUTE):
    network: InetAddress
    hop: InetAddress

@dataclass 
class RIP_ROUTE:
    version: int
    networks: list[InetAddress]
    passive_interfaces: list[str]
    is_default_information: bool

@dataclass
class VLAN_INFO:
    name : str
    number : int
    network_address : InetAddress
    gateway_address : InetAddress
    has_dhcp : bool 

#representa informacao de uma interface ou subinterface
class INTERFACE_INFO(ABC):
    def __init__(self, name : str, vlan: VLAN_INFO | None, description : str = "", shutdown : bool = True):
        self.name = name
        self.description = description
        self.shutdown = shutdown
        self.vlan = vlan

    @abstractmethod
    def generate_config_if(self, file=sys.stdout):
        config_if = ""
        if(self.description != ""): config_if += f"description {self.description}\n"
        if self.shutdown: config_if += "shutdown\n"
        else:config_if += "no shutdown\n"
        config_if +="exit\n\n"
        print(config_if, file=file, end='')

    def __str__(self):
        vlan = self.vlan
        if self.vlan is None: vlan = "''"
        return f"SWITCHPORT(name: {self.name}, vlan: {vlan}, shutdown: {self.description}, description: {self.description})"
    
    def __repr__(self) -> str:
        return self.__str__()

class SWITCHPORT_ACCESS_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "" , shutdown : bool = False):
        super().__init__(name, vlan, description, shutdown)

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"switchport mode access\n"
        config_if += f"switchport access vlan {self.vlan.number}\n"
        print(config_if, file=file, end='') 
        super().generate_config_if(file)
             
    

class SWITCHPORT_TRUNCK_INFO(INTERFACE_INFO):
    def __init__(self, name: str, description: str = "", shutdown: bool = False):
        super().__init__(name, None, description, shutdown)

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += "switchport trunk encapsulation dot1Q\n"
        config_if += f"switchport mode trunk\n"
        config_if += f"switchport trunk allowed vlan all\n"
        print(config_if, file=file, end='') 
        super().generate_config_if(file)

class INTERFACE_VLAN_INFO(INTERFACE_INFO):
    def __init__(self, vlan: VLAN_INFO, description: str = "", shutdown=False):
        super().__init__("", vlan, description, shutdown) 

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface vlan {self.vlan.number}\n"
        config_if += f"ip address {self.vlan.network_address.ip} {self.vlan.network_address.mask}\n"
        print(config_if, file=file,end='') 
        super().generate_config_if(file)

class ROUTER_INTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO | None, description: str = "", shutdown=True, clockrate: int | None = 64000 ):
        super().__init__(name, vlan, description, shutdown)
        self.clockrate = clockrate
        
    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        if self.vlan is not None:
            config_if += f"ip address {self.vlan.gateway_address.ip} {self.vlan.gateway_address.mask}\n"
        if self.clockrate is not None : config_if += f"clockrate {self.clockrate}\n"
        print(config_if, file=file, end='') 
        super().generate_config_if(file)

class ROUTER_SUBINTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "", encapsulation: str = 'dot1Q', shutdown : bool=False):
        assert encapsulation == 'dot1Q', f"Unsupported encapsulation {encapsulation}"
        super().__init__(name, vlan, description, shutdown)
        self.encapsulation = encapsulation

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"encapsulation {self.encapsulation} {self.vlan.number}\n"
        config_if += f"ip address {self.vlan.gateway_address.ip} {self.vlan.gateway_address.mask}\n"
        print(config_if, file=file, end='') 
        super().generate_config_if(file)


@dataclass
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

class DEVICE_INFO(ABC):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[INTERFACE_INFO], routes: list[ROUTE] = []):
        self.name = name
        self.config_info = config_info
        self.interfaces = interfaces
        self.routes = routes
        self.vlans : dict [str, VLAN_INFO]  = dict() #items are (key: vlan_name, value: VLAN_INFO)
        # cache the vlans known by the device
        for interface in self.interfaces:
            if interface.vlan is not None:
                vlan_name = interface.vlan.name
                vlan = interface.vlan
                if interface.vlan.name not in self.vlans.keys():
                    self.vlans[vlan_name] = vlan
    
    @abstractmethod
    def generate_basic_config(self, file=sys.stdout):
        basic_config=""
        basic_config += f"hostname {self.config_info.hostname}\n\n"
        if self.config_info.domain_name != '':
            basic_config += f"ip domain name {self.config_info.domain_name}\n\n"
        if self.config_info.password != '': 
            basic_config += f"enable secret {self.config_info.password}\n\n"
            basic_config += f"line console {self.config_info.line_console}\n"
            basic_config += f"password {self.config_info.password}\n"
            basic_config += f"login\n"
            if not self.config_info.enable_ssh: #because all the vty line will be hooked to ssh
                basic_config += f"exit\n\n"
                basic_config += f"line vty {self.config_info.line_vty[0]} {self.config_info.line_vty[1]}\n"
                basic_config += f"password {self.config_info.password}\n"
                basic_config += f"login\n"
                basic_config += f"exit\n\n"    
            basic_config += f"banner motd # {self.config_info.motd} #\n\n"
        if self.config_info.enable_ssh:
            basic_config += "crypto key generate rsa\n" #hardcode rsa
            basic_config += "ip ssh version 2\n"
            basic_config += f"line vty 0 15\n" #harcode the range
            basic_config += f"transport input ssh\n"
            basic_config += f"login local\n\n"
        print(basic_config, file=file,end='')

    @abstractmethod
    def generate_config_if(self, file = sys.stdout):
        for interface in self.interfaces:
            interface.generate_config_if(file)

    def generate_routes(self, file):
        for route in self.routes:
            if isinstance(route, STATIC_ROUTE):
                print(f"ip route {route.network.ip} {route.network.mask} {route.hop.ip}\n", file=file)
            elif isinstance(route, RIP_ROUTE):
                rip_config: str = ""
                rip_config += "router rip\n"
                rip_config += "version 2\n"
                if route.is_default_information:
                    rip_config += "default-information originate\n"
                for network in route.networks:
                    rip_config += f"network {network}\n"
                for interface in route.passive_interfaces:
                    rip_config += f"passive interface {interface}\n"
                rip_config += "exit\n"
                print(rip_config, file=file, end='')
            else:
                assert False, "Unreachable"
        print("\n",file=file)
    
    @abstractmethod
    def generate_config(self, file= sys.stdout):
        print("#" * 80, file=file)
        print(f"# Configuration for {self.name}\n\n", file=file, end='')
        print("configure terminal\n", file=file)
        self.generate_basic_config(file)
        self.generate_routes(file)
        print("end")
        print("copy running-config startup-config")
        print("")
        pass

    def __str__(self) -> str:
        return f"DEVICE_INFO(name: {self.name}, config: {self.config_info}, interfaces: {self.interfaces})"
    
    def __repr__(self) -> str:
        return self.__str__()

class SWITCH_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[SWITCHPORT_ACCESS_INFO | SWITCHPORT_TRUNCK_INFO | INTERFACE_VLAN_INFO], routes: list[ROUTE]):
        super().__init__(name, config_info, interfaces, routes)

    def generate_basic_config(self, file=sys.stdout):
        return super().generate_basic_config(file)
    
    def generate_config_if(self, file=sys.stdout):
        return super().generate_config_if(file)
    
    def generate_config(self, file = sys.stdout):        
        super().generate_config(file)
        
        #specific for switches
        #announce vlans
        for _ , vlan in self.vlans.items():
            vlan_info = ""
            vlan_info += f"vlan {vlan.number}\n"
            vlan_info += f"name {vlan.name}\n"
            vlan_info += f"exit\n\n"
            print(vlan_info, file=file)
        #config
        for interface in self.interfaces:
            interface.generate_config_if(file)



class ROUTER_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[ROUTER_INTERFACE_INFO | ROUTER_SUBINTERFACE_INFO], routes: list[ROUTE]):
        super().__init__(name, config_info, interfaces, routes)
      
    def generate_basic_config(self, file=sys.stdout):
        super().generate_basic_config(file)
    
    def generate_config_if(self, file=sys.stdout):
        super().generate_config_if(file)

    def generate_config(self, file=sys.stdout):
        super().generate_config(file)
        
        #specific for routers
        #generate dhcp config for vlans
        for _ , vlan in self.vlans.items():
            if vlan.has_dhcp:
                vlan_dhcp = ""
                vlan_dhcp += f"ip dhcp pool vlan{vlan.number}\n"
                vlan_dhcp += f"network {vlan.network_address.ip} {vlan.network_address.mask}\n"
                vlan_dhcp += f"default-router {vlan.gateway_address.ip}\n"
                vlan_dhcp += f"exit\n\n"
                print(vlan_dhcp, file=file, end='')
            
        #generate 
        for interface in self.interfaces:
            interface.generate_config_if(file)

    
def generate_config(file = sys.stdout, devices: list[SWITCH_INFO | ROUTER_INFO] = []):
    for device in devices:

        print("#"*80, file=file)
        print(f"Generating config for {device.name}", file=file)
        device.generate_config(file)
        print("", file=file)
    print("", file=file)
