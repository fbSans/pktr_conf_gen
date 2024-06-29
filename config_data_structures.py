from dataclasses import dataclass
from abc import ABC, abstractmethod
import sys

class InetAddress:
    def __init__(self, ip: str, mask: str):
        self.ip = ip
        self.mask: str = mask

class InetAddress4(InetAddress):
    def __init__(self, ipv4: str, mask: str):
        #add a check to ipv4
        super().__init__(ipv4, mask)
    
class InetAddress6(InetAddress):
    def __init__(self, ipv6: str, mask: str):
        #add a check to ipv6
        super().__init__(ipv6, mask)

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
    def __init__(self, name: str, description: str, shutdown: bool = False):
        super().__init__(name, None, description, shutdown)

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"switchport mode trunk\n"
        config_if += f"switchport trunk allowed vlan all\n"
        print(config_if, file=file, end='') 
        super().generate_config_if(file)

class INTERFACE_VLAN_INFO(INTERFACE_INFO):
    def __init__(self, vlan: VLAN_INFO, description: str, shutdown=False):
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
        if self.vlan is not None:
            config_if = f"interface {self.name}\n"
            config_if += f"ip address {self.vlan.gateway_address.ip} {self.vlan.gateway_address.mask}\n"
        if self.clockrate is not None : config_if += f"clockrate {self.clockrate}\n"
        print(config_if, file=file, end='') 
        super().generate_config_if(file)

class ROUTER_SUBINTERFACE_INFO(INTERFACE_INFO):
    def __init__(self, name: str, vlan: VLAN_INFO, description: str = "", encapsulation: str = 'dot1Q', shutdown : bool=True):
        assert encapsulation == 'dot1Q', f"Unsupported encapsulation {encapsulation}"
        super().__init__(name, vlan, description, shutdown)
        self.encapsulation = encapsulation

    def generate_config_if(self, file=sys.stdout):
        config_if = f"interface {self.name}\n"
        config_if += f"encapsulation {self.encapsulation}"
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
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[INTERFACE_INFO]):
        self.name = name
        self.config_info = config_info
        self.interfaces = interfaces
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
            basic_config += f"motd #{self.config_info.motd}#\n\n"
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
    
    @abstractmethod
    def generate_config(self, file= sys.stdout):
        print("#" * 50, file=file)
        print(f"# Configuracao para {self.name}\n\n", file=file, end='')
        print("configuration terminal\n", file=file)
        self.generate_basic_config(file)
        pass

class SWITCH_INFO (DEVICE_INFO):
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[SWITCHPORT_ACCESS_INFO | SWITCHPORT_TRUNCK_INFO | INTERFACE_VLAN_INFO]):
        super().__init__(name, config_info, interfaces)

    def generate_basic_config(self, file=sys.stdout):
        return super().generate_basic_config(file)
    
    def generate_config_if(self, file=sys.stdout):
        return super().generate_config_if(file)
    
    def generate_config(self, file = sys.stdout):        
        super().generate_config()
        
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
    def __init__(self, name: str, config_info: CONFIG_INFO, interfaces: list[ROUTER_INTERFACE_INFO | ROUTER_SUBINTERFACE_INFO]):
        super().__init__(name, config_info, interfaces)
      
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
    pass


if __name__ == '__main__':
    file=sys.stdout
    default_confg = CONFIG_INFO(
        hostname="RMaputo", 
        domain_name="homenet", 
        password="feuem", 
        motd="Acesso restrito !!!",
        line_console= 0, 
        line_vty=(1, 4), 
        enable_ssh=True, 
        ssh_password='uma senha de confianca, confia'
    )

    net10 = InetAddress4("192.168.10.0", "255.255.255.0")
    getway10 = InetAddress4("192.168.10.254", "255.255.255.0")
    vlan_10 = VLAN_INFO(name="clientes", number=10, network_address=net10, gateway_address=getway10, has_dhcp=True)
   
    fa_interface = ROUTER_INTERFACE_INFO(name='FastEthernet 0/0', vlan=vlan_10, description="Interface do gateway da vlan10", shutdown=False, clockrate=64000)
    router_info = ROUTER_INFO("R1", default_confg, [fa_interface])
    router_info.generate_config(file)

    print("", file=file)
    default_confg.hostname = "SMaputo"
    sw_fa_interface_range = SWITCHPORT_ACCESS_INFO(name="range FastEthernet0/1-5", vlan=vlan_10, description="interface de acesso para vlan 10", shutdown=False)
    vlan_10_1 = vlan_10
    vlan_10_1.network_address.ip = "192.168.10.1"
    interface_vlan10 = INTERFACE_VLAN_INFO(vlan_10, "", False)
    switch_info = SWITCH_INFO("S1", default_confg, [interface_vlan10, sw_fa_interface_range])
    switch_info.generate_config(file)