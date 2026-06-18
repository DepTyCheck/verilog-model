-- Seed: 16444222302122481189,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity vmy is
  port (tjmz : buffer std_logic_vector(1 to 1); twzjsyd : linkage time; pcxcl : inout real);
end vmy;

architecture bdxg of vmy is
  
begin
  -- Multi-driven assignments
  tjmz <= (others => 'W');
  tjmz <= (others => 'L');
  tjmz <= (others => 'W');
end bdxg;

entity i is
  port (gegmbucxg : buffer time_vector(0 downto 4); ala : linkage bit_vector(1 to 0); v : inout integer);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture crgal of i is
  signal rppbwvld : real;
  signal zskqs : time;
  signal eho : std_logic_vector(1 to 1);
begin
  ovije : entity work.vmy
    port map (tjmz => eho, twzjsyd => zskqs, pcxcl => rppbwvld);
  
  -- Single-driven assignments
  v <= 2_4_1_3_3;
  gegmbucxg <= (others => 0 ns);
  
  -- Multi-driven assignments
  eho <= "-";
  eho <= (others => '-');
end crgal;



-- Seed after: 148655263634933544,8118127366649987907
