-- Seed: 2471265820615334043,16715549879197889543



entity azcfuvzl is
  port (sgnpngui : out bit_vector(2 to 1); nch : in integer; qcszmsq : linkage integer_vector(1 downto 1));
end azcfuvzl;



architecture ztdofc of azcfuvzl is
  
begin
  
end ztdofc;



entity qxpexyhtu is
  port (bfrk : inout integer_vector(4 downto 2));
end qxpexyhtu;



architecture uly of qxpexyhtu is
  signal uxsi : bit_vector(2 to 1);
  signal ywmtu : integer;
  signal ndez : bit_vector(2 to 1);
  signal ycshiv : integer_vector(1 downto 1);
  signal io : integer;
  signal ykjcxoxc : bit_vector(2 to 1);
  signal vxyjfygr : integer_vector(1 downto 1);
  signal avzxrv : integer;
  signal xonczjlv : bit_vector(2 to 1);
begin
  esyxt : entity work.azcfuvzl
    port map (sgnpngui => xonczjlv, nch => avzxrv, qcszmsq => vxyjfygr);
  qsgtv : entity work.azcfuvzl
    port map (sgnpngui => ykjcxoxc, nch => io, qcszmsq => ycshiv);
  pwhbcingf : entity work.azcfuvzl
    port map (sgnpngui => ndez, nch => ywmtu, qcszmsq => ycshiv);
  debsy : entity work.azcfuvzl
    port map (sgnpngui => uxsi, nch => avzxrv, qcszmsq => vxyjfygr);
end uly;

library ieee;
use ieee.std_logic_1164.all;

entity kosyax is
  port (cddi : buffer integer; uk : inout character; rodxdr : linkage std_logic);
end kosyax;



architecture neivwbt of kosyax is
  signal b : integer_vector(4 downto 2);
begin
  jovkxo : entity work.qxpexyhtu
    port map (bfrk => b);
end neivwbt;



-- Seed after: 6869752847849998083,16715549879197889543
