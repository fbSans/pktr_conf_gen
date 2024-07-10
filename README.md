# TODO: Revise this README.md after the first usable version of the config generator

# Not Serious Packet Tracer Configuration Generator
## **About The config_gen.py:**
    This is a simple [script](./config_gen.py) with prints to its output stream packet trace configurations for the devices (routers or switches) that are provided. [add more details]

## Validation
    it will not try to make any sort of validation for the information you pass for names, addresses or masks. It is left for the user to pass the appropriate validations if needed.

## **Dealing with interfaces**
    This implementation does not care with the names you pass for the interfaces. So here are some tricks to generate some special interfaces
### Interface range
        1. For example to make configuration for **interface range FastEthernet 0/0-10** you pass the corresponding `INTERFACE_INFO` with name **range FastEthernet 0/0-10**.


