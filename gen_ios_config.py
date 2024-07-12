import sys
from ConfigParser import parse_config_file
from PTConfigDataStrucures import generate_config


def usage(file=sys.stderr):
    print("Usage:")
    print(f"    gen_ios_config <input_filepath> [output_filepath]")
    print(f"        input_filepath: file the contains the description of devices configuration.")
    print(f"        output_filepaht: optional path where the config will be printed on.")

if __name__ == '__main__':
    if len(sys.argv) < 2: 
        usage(sys.stderr)
        exit(1)
    input_file = sys.argv[1]
    output_filepath = None
    if len(sys.argv) > 2:
        output_filepath = sys.argv[2]

    devices = parse_config_file(input_file)
    if output_filepath is None:
        generate_config(devices=devices)
    else:
        with open(output_filepath, "+w") as f:
            generate_config(f, devices)
    