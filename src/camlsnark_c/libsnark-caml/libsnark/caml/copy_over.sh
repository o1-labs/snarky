sed 's/bn128/mnt4/g' < caml_bn128.cpp > caml_mnt4.cpp
sed 's/bn128/mnt6/g' < caml_bn128.cpp > caml_mnt6.cpp
sed 's/bn128/mnt4753/g' < caml_bn128.cpp > caml_mnt4753.cpp
sed 's/bn128/mnt6753/g' < caml_bn128.cpp > caml_mnt6753.cpp

sed 's/mnt4/mnt6/g' < caml_mnt4_specific.cpp > caml_mnt6_specific.cpp
sed 's/mnt4/mnt4753/g' < caml_mnt4_specific.cpp > caml_mnt4753_specific.cpp
sed 's/mnt4/mnt6753/g' < caml_mnt4_specific.cpp > caml_mnt6753_specific.cpp
