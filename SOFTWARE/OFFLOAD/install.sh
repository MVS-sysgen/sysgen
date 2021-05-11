# Generating OFFLOAD JCL

cat jcl_header.txt > 01_install_offload.jcl
cat \#* >> 01_install_offload.jcl
cat OFFLOADW.txt >> 01_install_offload.jcl
cat jcl_footer.txt >> 01_install_offload.jcl