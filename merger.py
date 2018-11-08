#!/usr/bin/python3
import os
import sys


src_root_path = ''
out_path = ''
out_filename = 'out.R'
script_dir = os.path.dirname(os.path.realpath(__file__))
print('merger CWD:', script_dir)

def get_real_path(*paths):
    return os.path.join(script_dir, *paths)


def find_all_source_files(root_dir):
    abs_root_dir = get_real_path(root_dir)
    if src_root_path != '' and not os.path.exists(abs_root_dir):
        raise Exception('Path {} has not been found'.format(abs_root_dir))
    print('Getting files from:', abs_root_dir)
    return os.listdir(abs_root_dir)


def file_formatter(filename, file_content):
    file_separator = '*' * 20
    return '#{}{}{}\n\n{}\n\n'.format(file_separator, filename, file_separator, file_content)


def merge_src_files(files_list, src_root_dir, out_path, out_filename):
    if len(files_list) < 1:
        print('No files to merge.')
        return
    else:
        print('Files to merge', files_list)
    abs_out_path = get_real_path(out_path)
    if out_path != '' and not os.path.exists(abs_out_path):
        raise Exception('Path {} doesn\'t exist'.format(abs_out_path))
    print('Merging to: {}/{}'.format(abs_out_path, out_filename))
    with open(get_real_path(out_path, out_filename), 'w') as file:
        for src_filename in files_list:
            with open(get_real_path(src_root_dir, src_filename)) as source:
                [file.write(file_formatter(src_filename, ''.join(source.readlines())))]
                


if __name__ == "__main__":
    if len(sys.argv) == 3:
        src_root_path = sys.argv[1]
        out_path = sys.argv[2]
    elif len(sys.argv) == 4:
        src_root_path = sys.argv[1]
        out_path = sys.argv[2]
        out_filename = sys.argv[3]
    else:
        print('usage: relative_source_files_path merged_file_path [merged_filename]')
        exit(1)
    try:
        source_files_list = find_all_source_files(src_root_path)
        merge_src_files(source_files_list, src_root_path, out_path, out_filename)
    except Exception as e:
        print("An error occured: {}".format(e))
        exit(1)