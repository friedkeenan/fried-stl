project('fried-stl', 'cpp',
    default_options : 'buildtype=release',
)

sources = []
subdir('source')

executable(meson.project_name(), sources,
    include_directories : 'include',

    link_args : '-s',
    cpp_args  : [
        '-std=gnu++2b',

        '-Wall',
        '-Wextra',
        '-Wpedantic',
        '-Wconversion',
        '-Werror',
    ],
)
