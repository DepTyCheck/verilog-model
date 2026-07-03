-- Seed: 281042192840037963,2158184632809654795

use std.reflection.all;

entity dug is
  port (nbjcpsbubf : inout physical_value_mirror; j : buffer integer; samntyzo : inout integer_vector(4 downto 0));
end dug;

architecture ioie of dug is
  
begin
  -- Single-driven assignments
  j <= j;
end ioie;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity imbxcc is
  port (yhbl : inout array_value_mirror; hd : inout integer_subtype_mirror; v : buffer std_logic_vector(3 to 3));
end imbxcc;

use std.reflection.all;

architecture sycift of imbxcc is
  signal dqjbspnbwz : integer_vector(4 downto 0);
  signal dloqrge : integer;
  shared variable mgpe : physical_value_mirror;
  signal k : integer_vector(4 downto 0);
  signal c : integer;
  shared variable b : physical_value_mirror;
begin
  ipcm : entity work.dug
    port map (nbjcpsbubf => b, j => c, samntyzo => k);
  kplrblmr : entity work.dug
    port map (nbjcpsbubf => mgpe, j => dloqrge, samntyzo => dqjbspnbwz);
  
  -- Multi-driven assignments
  v <= "Z";
end sycift;

library ieee;
use ieee.std_logic_1164.all;

entity hfrxlck is
  port (txaoyag : out std_logic; h : out std_logic; sqcqklbiq : buffer real_vector(2 downto 1));
end hfrxlck;

use std.reflection.all;

architecture teytys of hfrxlck is
  signal ls : integer_vector(4 downto 0);
  signal eaori : integer;
  shared variable uegu : physical_value_mirror;
  signal pn : integer_vector(4 downto 0);
  signal eqik : integer;
  shared variable bf : physical_value_mirror;
  signal n : integer_vector(4 downto 0);
  signal dv : integer;
  shared variable js : physical_value_mirror;
begin
  hbaztwbcp : entity work.dug
    port map (nbjcpsbubf => js, j => dv, samntyzo => n);
  zxwngekij : entity work.dug
    port map (nbjcpsbubf => bf, j => eqik, samntyzo => pn);
  vmtn : entity work.dug
    port map (nbjcpsbubf => uegu, j => eaori, samntyzo => ls);
  
  -- Single-driven assignments
  sqcqklbiq <= (8#7.2_3_7#, 4.4_3_3);
  
  -- Multi-driven assignments
  h <= h;
  h <= txaoyag;
end teytys;



-- Seed after: 4056508923649946115,2158184632809654795
