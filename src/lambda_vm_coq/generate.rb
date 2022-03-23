# Generate the OCaml files to Coq using `coq-of-ocaml`

if ARGV.size < 2 then
  puts "Usage:"
  puts "  ruby generate.rb deku_path vm_path"
  exit(1)
end

deku_path, vm_path = ARGV
full_path = File.join(deku_path, vm_path)

# Generate to Coq by coq-of-ocaml
mli_files = [ ]
generate_files =
  Dir.glob(File.join(full_path, "*.ml")) +   
  mli_files.map {|path| File.join(full_path, path)}
for ocaml_file_name in generate_files.sort do
    command = "cd #{full_path} && coq-of-ocaml #{File.basename(ocaml_file_name)}"
  system(command)
end

# Generate proofs by easier-proofs
coq_files =
  Dir.glob(File.join(full_path, "*.v")) 
for coq_file_name in coq_files.sort do
    command = "cd #{full_path} && mv #{File.basename(coq_file_name)} #{deku_path}/src/lambda_vm_coq"
  system(command)
end

system("rsync --checksum #{full_path}/*.v #{vm_coq_path}") 
system("rm #{full_path}/*.v")

# Generate _CoqProject
system("./configure.sh")