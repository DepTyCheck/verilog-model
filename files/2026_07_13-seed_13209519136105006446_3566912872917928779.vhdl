-- Seed: 13209519136105006446,3566912872917928779

entity ercddd is
  port (zexvxnxu : buffer real_vector(1 to 2));
end ercddd;

architecture ltxtsu of ercddd is
  
begin
  
end ltxtsu;

use std.reflection.all;

entity mpa is
  port (muqwvxmun : inout floating_value_mirror; txhlsxi : buffer boolean_vector(1 to 2); trim : out boolean);
end mpa;

architecture otojoxnp of mpa is
  signal ch : real_vector(1 to 2);
  signal vgwiaxsknd : real_vector(1 to 2);
begin
  wzsxa : entity work.ercddd
    port map (zexvxnxu => vgwiaxsknd);
  wydx : entity work.ercddd
    port map (zexvxnxu => ch);
  
  -- Single-driven assignments
  trim <= trim;
  txhlsxi <= txhlsxi;
end otojoxnp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tswbzdair is
  port (zndq : linkage boolean; vfbvshw : out std_logic; digsx : inout floating_subtype_mirror; tonyzl : inout protected_subtype_mirror);
end tswbzdair;

architecture pfho of tswbzdair is
  signal yjziiqpp : real_vector(1 to 2);
begin
  oprvbewnk : entity work.ercddd
    port map (zexvxnxu => yjziiqpp);
  
  -- Multi-driven assignments
  vfbvshw <= 'W';
  vfbvshw <= '0';
  vfbvshw <= 'W';
  vfbvshw <= vfbvshw;
end pfho;



-- Seed after: 4998989684099214811,3566912872917928779
