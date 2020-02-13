import os

name_suffix = "-suffix"

basedir = "/home/thelmuth/Collab/thelmuth/Results/parent-selection-v3-UMAD/co-solvability/%s/"
other_push_args = ":parent-selection :lexicase :genetic-operator-probabilities {:uniform-addition-and-deletion,1}"


run_numbers = "0-99"

problems = ["compare-string-lengths",
            "double-letters",
            "replace-space-with-newline",
            "string-lengths-backwards",
            "last-index-of-zero",
            "vector-average",
            "mirror-image",
            "x-word-lines",
            "negative-to-zero",
            "scrabble-score",
            "smallest",
            "syllables"
]

#problems = ["vector-average"]

## Probably don't need to change below here.

with open('hpc_launcher.template', 'r') as hpc_template:
    hpc_launcher_template = hpc_template.read()

for problem in problems:
 
    namespace_dir = basedir % problem

    hpc_launcher = hpc_launcher_template.replace("#qsub-name#", problem + name_suffix)
    hpc_launcher = hpc_launcher.replace("#namespace#", problem)
    hpc_launcher = hpc_launcher.replace("#dir#", namespace_dir)
    hpc_launcher = hpc_launcher.replace("#run-numbers#", run_numbers)
    hpc_launcher = hpc_launcher.replace("#other-push-args#", other_push_args)

    temp_filename = "temp_launcher_%s.run" % problem
    with open(temp_filename, 'w') as temp_launcher:
        temp_launcher.write(hpc_launcher)

    os.system("qsub " + temp_filename)
    os.remove(temp_filename)
