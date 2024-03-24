##CALL sh
##CALL txt
function countFiles {
  ##HIDE
  ls *.$1 2>/dev/null | wc -l
  ##EDIT ...
}
