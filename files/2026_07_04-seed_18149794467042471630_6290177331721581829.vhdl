-- Seed: 18149794467042471630,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity cxhdcd is
  port (szdn : inout file_value_mirror; bwbxieo : linkage std_logic_vector(4 downto 3); rbxqep : buffer real);
end cxhdcd;

architecture pctqdh of cxhdcd is
  
begin
  -- Single-driven assignments
  rbxqep <= rbxqep;
end pctqdh;

use std.reflection.all;

entity wmgloda is
  port (fnlrekqcrq : inout value_mirror);
end wmgloda;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gxsnd of wmgloda is
  signal remick : real;
  signal m : std_logic_vector(4 downto 3);
  shared variable zdcci : file_value_mirror;
begin
  ekn : entity work.cxhdcd
    port map (szdn => zdcci, bwbxieo => m, rbxqep => remick);
  
  -- Multi-driven assignments
  m <= "WX";
end gxsnd;

use std.reflection.all;

entity ye is
  port (q : inout floating_value_mirror; cezyngzmr : inout enumeration_subtype_mirror);
end ye;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ac of ye is
  signal ombewtmf : real;
  signal sueapzjr : std_logic_vector(4 downto 3);
  shared variable bmdfskue : file_value_mirror;
begin
  num : entity work.cxhdcd
    port map (szdn => bmdfskue, bwbxieo => sueapzjr, rbxqep => ombewtmf);
end ac;

library ieee;
use ieee.std_logic_1164.all;

entity tpxgofzdno is
  port (v : out std_logic);
end tpxgofzdno;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture tmgv of tpxgofzdno is
  signal yogqrzsw : real;
  shared variable u : file_value_mirror;
  signal b : real;
  shared variable zr : file_value_mirror;
  signal bqhwaqx : real;
  signal jeyn : std_logic_vector(4 downto 3);
  shared variable hvb : file_value_mirror;
  signal iz : real;
  signal pu : std_logic_vector(4 downto 3);
  shared variable il : file_value_mirror;
begin
  q : entity work.cxhdcd
    port map (szdn => il, bwbxieo => pu, rbxqep => iz);
  mgtl : entity work.cxhdcd
    port map (szdn => hvb, bwbxieo => jeyn, rbxqep => bqhwaqx);
  dzj : entity work.cxhdcd
    port map (szdn => zr, bwbxieo => pu, rbxqep => b);
  wuatl : entity work.cxhdcd
    port map (szdn => u, bwbxieo => pu, rbxqep => yogqrzsw);
  
  -- Multi-driven assignments
  jeyn <= ('W', 'X');
  v <= v;
  jeyn <= jeyn;
  v <= v;
end tmgv;



-- Seed after: 703971635453838303,6290177331721581829
