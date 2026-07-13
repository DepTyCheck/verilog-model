-- Seed: 14015271227345202245,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity ngptkkmwq is
  port (ecsbypzqcj : inout time_vector(0 downto 1); itj : in std_logic);
end ngptkkmwq;

architecture vxwxl of ngptkkmwq is
  
begin
  -- Single-driven assignments
  ecsbypzqcj <= (others => 0 ns);
end vxwxl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vqkfomr is
  port (hzdxg : inout floating_subtype_mirror; syocroet : out std_logic_vector(1 to 2); mzprxxpqy : inout physical_value_mirror);
end vqkfomr;

library ieee;
use ieee.std_logic_1164.all;

architecture do of vqkfomr is
  signal vxvsoam : std_logic;
  signal sfwsp : time_vector(0 downto 1);
begin
  si : entity work.ngptkkmwq
    port map (ecsbypzqcj => sfwsp, itj => vxvsoam);
  
  -- Multi-driven assignments
  vxvsoam <= 'X';
  syocroet <= "0X";
end do;

entity m is
  port (h : inout severity_level);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture hy of m is
  signal fncns : std_logic;
  signal shdcq : time_vector(0 downto 1);
  signal y : time_vector(0 downto 1);
  signal awdlphw : time_vector(0 downto 1);
  signal vdrk : std_logic;
  signal tcpjz : time_vector(0 downto 1);
begin
  qsmingiy : entity work.ngptkkmwq
    port map (ecsbypzqcj => tcpjz, itj => vdrk);
  modd : entity work.ngptkkmwq
    port map (ecsbypzqcj => awdlphw, itj => vdrk);
  ooccnfzx : entity work.ngptkkmwq
    port map (ecsbypzqcj => y, itj => vdrk);
  zhsbgjxj : entity work.ngptkkmwq
    port map (ecsbypzqcj => shdcq, itj => fncns);
  
  -- Single-driven assignments
  h <= h;
  
  -- Multi-driven assignments
  vdrk <= '0';
end hy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zogrx is
  port (qvusuhc : inout integer_value_mirror; rwefmi : inout record_subtype_mirror; ka : in std_logic_vector(3 to 0));
end zogrx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mgt of zogrx is
  shared variable elktyts : physical_value_mirror;
  signal nul : std_logic_vector(1 to 2);
  shared variable afxakfra : floating_subtype_mirror;
  signal bfkzdcfws : std_logic;
  signal gqjrsbyd : time_vector(0 downto 1);
begin
  w : entity work.ngptkkmwq
    port map (ecsbypzqcj => gqjrsbyd, itj => bfkzdcfws);
  ay : entity work.vqkfomr
    port map (hzdxg => afxakfra, syocroet => nul, mzprxxpqy => elktyts);
  
  -- Multi-driven assignments
  bfkzdcfws <= bfkzdcfws;
end mgt;



-- Seed after: 3199012615614563056,3566912872917928779
