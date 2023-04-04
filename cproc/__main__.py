from cproc.args import handle_args
from cproc.process import process_files


opts = handle_args()
process_files(opts)
