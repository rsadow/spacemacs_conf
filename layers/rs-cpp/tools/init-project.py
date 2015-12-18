#!/usr/bin/env python

import sys
import os
import argparse
import shutil
import subprocess

def print_call():
    print ' -- pitcp call --'
    print ' ' + ' '.join(sys.argv)
    print ' -------------------'

if __name__ == "__main__":
    origPath = os.getcwd()
    cc_json = "compile_commands.json"

    parser = argparse.ArgumentParser();
    parser.add_argument("--src", help="path to src")
    parser.add_argument("--dst", help="path to dst")
    args = parser.parse_args()

    src_path = os.path.abspath(args.src)
    dst_path = os.path.abspath(args.dst)
    shutil.copy(os.path.join(src_path, cc_json), dst_path)
    subprocess.call(['sed', '-i', '-E', '-e', 's/--sysroot[^ ]+|-D__CCS_INLINE__[^ ]+//g', os.path.join(dst_path, cc_json)])
    subprocess.call(['sed', '-i', '-E', '-e', 's/\/bin.*proxy/\/usr\/bin\/c\+\+/g', os.path.join(dst_path, cc_json)])
    # print os.path.join(dst_path, cc_json)

    # parser.add_argument("--repo", help="path to repo", default=".")
    # parser.add_argument("--pit", help="path to pit")
    # args = parser.parse_args()

    # try:
    #     bin_path = "/bin/LinuxX86-64_WMP_FCT/SC_MONOLITH/"
    #     binary_name = "BTSOMexe";
    #     binary = args.repo + bin_path+ binary_name
    #     copy_to = args.pit + bin_path

    #     yshutil.copy(binary, copy_to)

    #     print binary + " copied to " + copy_to

    # except BadUsage:
    #     print_call()
