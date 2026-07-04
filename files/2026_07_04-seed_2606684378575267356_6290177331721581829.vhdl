-- Seed: 2606684378575267356,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wug is
  port ( ikyukrxg : out std_logic
  ; gbinldl : buffer std_logic_vector(4 downto 2)
  ; oiitkizz : inout physical_value_mirror
  ; isb : inout enumeration_value_mirror
  );
end wug;

architecture zyu of wug is
  
begin
  -- Multi-driven assignments
  gbinldl <= gbinldl;
  ikyukrxg <= '1';
  ikyukrxg <= 'H';
  ikyukrxg <= ikyukrxg;
end zyu;

use std.reflection.all;

entity kjxcms is
  port (qfvh : inout enumeration_value_mirror; qkqyl : inout integer; aqvhk : buffer time);
end kjxcms;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture wavzeyaz of kjxcms is
  shared variable hpaapexzi : enumeration_value_mirror;
  shared variable nhuycbx : physical_value_mirror;
  shared variable zdbcekzsvg : enumeration_value_mirror;
  shared variable ksfkj : physical_value_mirror;
  signal iduaxu : std_logic_vector(4 downto 2);
  signal eyaxzfnn : std_logic;
begin
  lsn : entity work.wug
    port map (ikyukrxg => eyaxzfnn, gbinldl => iduaxu, oiitkizz => ksfkj, isb => zdbcekzsvg);
  nz : entity work.wug
    port map (ikyukrxg => eyaxzfnn, gbinldl => iduaxu, oiitkizz => nhuycbx, isb => hpaapexzi);
  
  -- Single-driven assignments
  aqvhk <= 2#1011# ps;
  qkqyl <= qkqyl;
  
  -- Multi-driven assignments
  eyaxzfnn <= eyaxzfnn;
  iduaxu <= "XZZ";
end wavzeyaz;



-- Seed after: 3004595038303545582,6290177331721581829
