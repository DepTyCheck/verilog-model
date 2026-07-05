-- Seed: 10334651216209001087,3181554006726329157

use std.reflection.all;

entity nwde is
  port (lipj : inout integer_subtype_mirror; zcbnwwxhdr : inout record_value_mirror; tqkay : inout integer_subtype_mirror);
end nwde;

architecture mkxrhcabi of nwde is
  
begin
  
end mkxrhcabi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zsl is
  port (xddmyd : buffer std_logic_vector(2 to 0); y : inout protected_subtype_mirror; dfmzrv : buffer integer; cdtefiox : inout file_value_mirror);
end zsl;

use std.reflection.all;

architecture qjyhdcfmsj of zsl is
  shared variable zrdwpywjqv : integer_subtype_mirror;
  shared variable exmones : record_value_mirror;
  shared variable aeqh : integer_subtype_mirror;
  shared variable xqzxdi : integer_subtype_mirror;
  shared variable adv : record_value_mirror;
  shared variable bklbpwct : integer_subtype_mirror;
begin
  opxl : entity work.nwde
    port map (lipj => bklbpwct, zcbnwwxhdr => adv, tqkay => xqzxdi);
  lqnkrv : entity work.nwde
    port map (lipj => aeqh, zcbnwwxhdr => exmones, tqkay => zrdwpywjqv);
  
  -- Single-driven assignments
  dfmzrv <= dfmzrv;
end qjyhdcfmsj;

use std.reflection.all;

entity f is
  port (gwideenhhl : inout array_subtype_mirror; lazvhg : inout physical_value_mirror);
end f;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xolsdciefk of f is
  shared variable lyotn : integer_subtype_mirror;
  shared variable cct : record_value_mirror;
  shared variable gz : integer_subtype_mirror;
  shared variable xizi : integer_subtype_mirror;
  shared variable hf : record_value_mirror;
  shared variable lvsj : integer_subtype_mirror;
  shared variable rrkissj : file_value_mirror;
  signal kbufzr : integer;
  shared variable gpih : protected_subtype_mirror;
  signal qm : std_logic_vector(2 to 0);
  shared variable purkuglpb : integer_subtype_mirror;
  shared variable zknsi : record_value_mirror;
  shared variable nlizdeco : integer_subtype_mirror;
begin
  flajlh : entity work.nwde
    port map (lipj => nlizdeco, zcbnwwxhdr => zknsi, tqkay => purkuglpb);
  ebi : entity work.zsl
    port map (xddmyd => qm, y => gpih, dfmzrv => kbufzr, cdtefiox => rrkissj);
  gfgxm : entity work.nwde
    port map (lipj => lvsj, zcbnwwxhdr => hf, tqkay => xizi);
  qeez : entity work.nwde
    port map (lipj => gz, zcbnwwxhdr => cct, tqkay => lyotn);
  
  -- Multi-driven assignments
  qm <= qm;
  qm <= qm;
  qm <= "";
end xolsdciefk;



-- Seed after: 6181641941399416641,3181554006726329157
