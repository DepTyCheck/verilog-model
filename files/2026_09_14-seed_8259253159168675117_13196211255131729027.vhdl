-- Seed: 8259253159168675117,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity ayyfrxdshl is
  port (dhydttbx : in bit_vector(3 downto 1); fsy : inout std_logic_vector(1 to 0));
end ayyfrxdshl;

architecture netw of ayyfrxdshl is
  
begin
  -- Multi-driven assignments
  fsy <= fsy;
  fsy <= "";
  fsy <= (others => '0');
end netw;

entity sayfk is
  port (cdzu : linkage time);
end sayfk;

architecture oizflxof of sayfk is
  
begin
  
end oizflxof;

library ieee;
use ieee.std_logic_1164.all;

entity czjxdvdk is
  port (ixl : linkage integer; bxibjn : out string(3 downto 5); djhrnevuns : linkage std_logic; gjwcyln : inout boolean);
end czjxdvdk;

library ieee;
use ieee.std_logic_1164.all;

architecture vhh of czjxdvdk is
  signal zja : bit_vector(3 downto 1);
  signal nijhemx : std_logic_vector(1 to 0);
  signal unlmgjxj : bit_vector(3 downto 1);
begin
  kxuez : entity work.ayyfrxdshl
    port map (dhydttbx => unlmgjxj, fsy => nijhemx);
  xqnebp : entity work.ayyfrxdshl
    port map (dhydttbx => zja, fsy => nijhemx);
  
  -- Single-driven assignments
  gjwcyln <= TRUE;
  zja <= ('0', '0', '0');
  unlmgjxj <= zja;
  bxibjn <= bxibjn;
  
  -- Multi-driven assignments
  nijhemx <= "";
end vhh;

library ieee;
use ieee.std_logic_1164.all;

entity ehuejhpr is
  port (rjudcuw : buffer std_logic; enttuij : linkage boolean; agprbf : out std_logic; iigagcj : out real);
end ehuejhpr;

library ieee;
use ieee.std_logic_1164.all;

architecture znoyggkak of ehuejhpr is
  signal xdb : std_logic_vector(1 to 0);
  signal gvlreagg : bit_vector(3 downto 1);
  signal ymgivys : time;
begin
  iebdiydh : entity work.sayfk
    port map (cdzu => ymgivys);
  izdqh : entity work.ayyfrxdshl
    port map (dhydttbx => gvlreagg, fsy => xdb);
  
  -- Single-driven assignments
  iigagcj <= iigagcj;
  
  -- Multi-driven assignments
  agprbf <= '1';
end znoyggkak;



-- Seed after: 9332070673260174928,13196211255131729027
