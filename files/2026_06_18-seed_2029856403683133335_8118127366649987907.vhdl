-- Seed: 2029856403683133335,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity oemrsubf is
  port (vkwlpxwvn : buffer real; ejaitn : buffer std_logic; avce : buffer integer);
end oemrsubf;

architecture c of oemrsubf is
  
begin
  -- Single-driven assignments
  avce <= 8#4_3_0#;
  vkwlpxwvn <= 4.1_2_0_3_3;
  
  -- Multi-driven assignments
  ejaitn <= '-';
  ejaitn <= '0';
  ejaitn <= 'H';
  ejaitn <= 'L';
end c;

entity mjdc is
  port (iept : out real; autkghws : in real_vector(0 to 2));
end mjdc;

library ieee;
use ieee.std_logic_1164.all;

architecture cwvzw of mjdc is
  signal hyyenmav : integer;
  signal ghnupx : std_logic;
  signal welg : integer;
  signal jum : real;
  signal gj : integer;
  signal vxcl : std_logic;
  signal cvicmqo : real;
begin
  a : entity work.oemrsubf
    port map (vkwlpxwvn => cvicmqo, ejaitn => vxcl, avce => gj);
  wwzkaharjr : entity work.oemrsubf
    port map (vkwlpxwvn => jum, ejaitn => vxcl, avce => welg);
  jacsixh : entity work.oemrsubf
    port map (vkwlpxwvn => iept, ejaitn => ghnupx, avce => hyyenmav);
  
  -- Multi-driven assignments
  ghnupx <= 'U';
  vxcl <= 'Z';
end cwvzw;



-- Seed after: 2929947821793300775,8118127366649987907
