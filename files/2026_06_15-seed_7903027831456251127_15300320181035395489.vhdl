-- Seed: 7903027831456251127,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity yohtjmegkp is
  port (ab : inout std_logic_vector(1 to 1); niuzipdf : inout integer; qyel : in integer; e : inout std_logic_vector(1 downto 4));
end yohtjmegkp;

architecture hqebmkb of yohtjmegkp is
  
begin
  -- Single-driven assignments
  niuzipdf <= 16#3_7_2_6#;
  
  -- Multi-driven assignments
  e <= (others => '0');
end hqebmkb;

entity yzfgqlszf is
  port (lihdfdz : buffer integer);
end yzfgqlszf;

library ieee;
use ieee.std_logic_1164.all;

architecture pb of yzfgqlszf is
  signal godyagxgu : std_logic_vector(1 to 1);
  signal klkao : std_logic_vector(1 downto 4);
  signal aospnej : integer;
  signal iktia : std_logic_vector(1 to 1);
begin
  jx : entity work.yohtjmegkp
    port map (ab => iktia, niuzipdf => aospnej, qyel => lihdfdz, e => klkao);
  qrwhswnu : entity work.yohtjmegkp
    port map (ab => godyagxgu, niuzipdf => lihdfdz, qyel => aospnej, e => klkao);
  
  -- Multi-driven assignments
  iktia <= (others => 'Z');
  iktia <= "U";
end pb;



-- Seed after: 12011809017530587693,15300320181035395489
