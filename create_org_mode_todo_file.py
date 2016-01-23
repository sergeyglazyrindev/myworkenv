#! /usr/bin/python

import sys
import re
import os


def run():
    lines = sys.stdin.readlines()
    try:
        with open(os.getenv('PROJECTS_ROOT') + 'todo.org', 'r') as fp:
            current_todo = fp.read()
    except IOError:
        current_todo = ''

    todo = []
    for line in lines:
        matches = re.search(r'^(.*)?:(\d+):\s+(.*)$', line)
        current_file = re.sub(r'^\.\/', '', matches.group(1))
        line_in_file = matches.group(2)
        description = matches.group(3)
        importance = re.search(r'\@tod(o+)', description)
        importance = len(importance.group(1))
        description = re.sub(r'.*\@todo+\s*', '', description)
        if '[[file:{file}::{line}]]'.format(file=current_file, line=line_in_file) in current_todo:
            continue
        todo.append({
            'importance': importance,
            'description': description,
            'file': current_file,
            'line': line_in_file
        })
    todo.sort(key=lambda x: x['importance'], reverse=True)
    todo_buffer = []
    for i, _todo in enumerate(todo):
        todo_item = '''{description}
[[file:{file}::{line}]]
'''.format(i=i+1, **_todo)
        todo_buffer.append(todo_item)
    print '{}{}'.format(current_todo, ''.join(todo_buffer))

run()
