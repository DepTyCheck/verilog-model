-- Seed: 7701417802917022444,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity blgfxmbko is
  port (bikylcjxg : buffer std_logic_vector(1 to 1));
end blgfxmbko;

architecture rcfydhw of blgfxmbko is
  
begin
  
end rcfydhw;

use std.reflection.all;

entity bxebdrkl is
  port (mr : inout access_value_mirror; tykejucae : inout file_value_mirror);
end bxebdrkl;

library ieee;
use ieee.std_logic_1164.all;

architecture n of bxebdrkl is
  signal ebondqmnev : std_logic_vector(1 to 1);
begin
  bkkspvrofd : entity work.blgfxmbko
    port map (bikylcjxg => ebondqmnev);
  lhteb : entity work.blgfxmbko
    port map (bikylcjxg => ebondqmnev);
  wyjl : entity work.blgfxmbko
    port map (bikylcjxg => ebondqmnev);
end n;

library ieee;
use ieee.std_logic_1164.all;

entity chyinloybu is
  port (zkd : buffer std_logic; g : inout time_vector(3 to 0); bxpstdow : buffer std_logic_vector(3 to 3); vrmtte : out real);
end chyinloybu;

use std.reflection.all;

architecture mbytgidypr of chyinloybu is
  shared variable qv : file_value_mirror;
  shared variable w : access_value_mirror;
begin
  zqatckqkt : entity work.bxebdrkl
    port map (mr => w, tykejucae => qv);
  
  -- Single-driven assignments
  vrmtte <= 0_2_2_1_2.0_0_4_2;
  g <= (others => 0 ns);
  
  -- Multi-driven assignments
  bxpstdow <= (others => 'Z');
  bxpstdow <= "U";
  zkd <= zkd;
  bxpstdow <= bxpstdow;
end mbytgidypr;



-- Seed after: 5899006397985813365,7726014785203345639
