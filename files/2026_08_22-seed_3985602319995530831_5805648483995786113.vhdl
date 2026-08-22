-- Seed: 3985602319995530831,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity kpdmz is
  port (viss : out std_logic_vector(3 to 0); af : in real_vector(2 downto 2));
end kpdmz;

architecture xuxbxdxs of kpdmz is
  
begin
  -- Multi-driven assignments
  viss <= (others => '0');
  viss <= viss;
  viss <= viss;
end xuxbxdxs;

entity unbfvl is
  port (tflvqurxg : inout time; vemmn : inout bit_vector(4 to 4); rjgzolj : inout real);
end unbfvl;

library ieee;
use ieee.std_logic_1164.all;

architecture skjrcoor of unbfvl is
  signal nje : std_logic_vector(3 to 0);
  signal gwlylyf : real_vector(2 downto 2);
  signal wbme : std_logic_vector(3 to 0);
begin
  vmvpp : entity work.kpdmz
    port map (viss => wbme, af => gwlylyf);
  ducjqsn : entity work.kpdmz
    port map (viss => nje, af => gwlylyf);
  pw : entity work.kpdmz
    port map (viss => nje, af => gwlylyf);
  
  -- Single-driven assignments
  rjgzolj <= rjgzolj;
  gwlylyf <= gwlylyf;
  vemmn <= (others => '1');
  tflvqurxg <= tflvqurxg;
  
  -- Multi-driven assignments
  nje <= wbme;
  wbme <= wbme;
  nje <= wbme;
end skjrcoor;



-- Seed after: 9935520121544014678,5805648483995786113
