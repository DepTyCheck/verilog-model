-- Seed: 5196388270196029904,3181554006726329157

use std.reflection.all;

entity n is
  port (jictutcn : inout value_mirror; mlprahy : inout integer_value_mirror);
end n;

architecture odurnuk of n is
  
begin
  
end odurnuk;

library ieee;
use ieee.std_logic_1164.all;

entity puncrafcb is
  port (htnrhirv : out std_logic; f : linkage integer; dp : out bit_vector(3 to 2));
end puncrafcb;

use std.reflection.all;

architecture wpwogp of puncrafcb is
  shared variable idfsx : integer_value_mirror;
  shared variable hj : value_mirror;
  shared variable jslb : integer_value_mirror;
  shared variable lenopdubpx : value_mirror;
  shared variable h : integer_value_mirror;
  shared variable nedbng : value_mirror;
begin
  hjlndv : entity work.n
    port map (jictutcn => nedbng, mlprahy => h);
  kaixd : entity work.n
    port map (jictutcn => lenopdubpx, mlprahy => jslb);
  jqresegcv : entity work.n
    port map (jictutcn => hj, mlprahy => idfsx);
  
  -- Single-driven assignments
  dp <= (others => '0');
end wpwogp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity dyvfeb is
  port (vsmnuaqr : inout file_value_mirror; dqiluvrvw : in std_logic_vector(0 downto 2));
end dyvfeb;

library ieee;
use ieee.std_logic_1164.all;

architecture u of dyvfeb is
  signal jzlautxbrz : bit_vector(3 to 2);
  signal aprqdf : integer;
  signal ef : std_logic;
  signal f : bit_vector(3 to 2);
  signal vohe : integer;
  signal fwczozizfo : std_logic;
begin
  hykpvqfd : entity work.puncrafcb
    port map (htnrhirv => fwczozizfo, f => vohe, dp => f);
  zievr : entity work.puncrafcb
    port map (htnrhirv => ef, f => aprqdf, dp => jzlautxbrz);
  
  -- Multi-driven assignments
  ef <= 'L';
  ef <= '1';
  fwczozizfo <= fwczozizfo;
  fwczozizfo <= 'U';
end u;

use std.reflection.all;

entity razvbn is
  port (ucqzo : in real; ddnfa : out time; d : inout file_subtype_mirror);
end razvbn;

library ieee;
use ieee.std_logic_1164.all;

architecture kjcl of razvbn is
  signal r : bit_vector(3 to 2);
  signal c : integer;
  signal myrpjuw : std_logic;
begin
  w : entity work.puncrafcb
    port map (htnrhirv => myrpjuw, f => c, dp => r);
  
  -- Single-driven assignments
  ddnfa <= 3.1_1_0_4_0 fs;
  
  -- Multi-driven assignments
  myrpjuw <= myrpjuw;
  myrpjuw <= 'X';
  myrpjuw <= myrpjuw;
end kjcl;



-- Seed after: 12750751414351250638,3181554006726329157
