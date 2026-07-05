-- Seed: 17521344369190784172,3181554006726329157

use std.reflection.all;

entity sidhtd is
  port (iam : inout record_subtype_mirror; qspkqbx : in time_vector(0 to 1));
end sidhtd;

architecture lpyrcqkhsz of sidhtd is
  
begin
  
end lpyrcqkhsz;

library ieee;
use ieee.std_logic_1164.all;

entity idfzmhhp is
  port (qukebfsuk : buffer std_logic_vector(0 to 0));
end idfzmhhp;

use std.reflection.all;

architecture uu of idfzmhhp is
  signal czzohuwav : time_vector(0 to 1);
  shared variable jc : record_subtype_mirror;
begin
  rfbrwwjf : entity work.sidhtd
    port map (iam => jc, qspkqbx => czzohuwav);
  
  -- Single-driven assignments
  czzohuwav <= (1 min, 4 min);
  
  -- Multi-driven assignments
  qukebfsuk <= qukebfsuk;
end uu;

use std.reflection.all;

entity qe is
  port (dvnyoh : linkage integer_vector(2 downto 4); a : inout access_subtype_mirror; dithrbbry : inout file_subtype_mirror);
end qe;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture wx of qe is
  shared variable voztziw : record_subtype_mirror;
  signal r : std_logic_vector(0 to 0);
  shared variable bagjkr : record_subtype_mirror;
  signal gpcfpsxdkv : time_vector(0 to 1);
  shared variable l : record_subtype_mirror;
begin
  fyhyvhhfyj : entity work.sidhtd
    port map (iam => l, qspkqbx => gpcfpsxdkv);
  eihspqieb : entity work.sidhtd
    port map (iam => bagjkr, qspkqbx => gpcfpsxdkv);
  egrzxefeqc : entity work.idfzmhhp
    port map (qukebfsuk => r);
  upemc : entity work.sidhtd
    port map (iam => voztziw, qspkqbx => gpcfpsxdkv);
  
  -- Single-driven assignments
  gpcfpsxdkv <= gpcfpsxdkv;
  
  -- Multi-driven assignments
  r <= "W";
  r <= (others => 'X');
  r <= r;
  r <= "Z";
end wx;



-- Seed after: 12835304412809294757,3181554006726329157
