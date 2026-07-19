-- Seed: 3668480564735678896,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity jjfl is
  port (sppednpa : out real; ocgelx : inout std_logic_vector(3 to 3); vd : buffer std_logic_vector(4 downto 0));
end jjfl;

architecture tq of jjfl is
  
begin
  -- Single-driven assignments
  sppednpa <= 16#F_B_0.9_2_6_5_6#;
  
  -- Multi-driven assignments
  vd <= vd;
end tq;

entity crimbjtejr is
  port (xbbaxyiwx : inout integer);
end crimbjtejr;

library ieee;
use ieee.std_logic_1164.all;

architecture wyzz of crimbjtejr is
  signal vtqa : std_logic_vector(4 downto 0);
  signal ieyizvmu : real;
  signal ufz : std_logic_vector(4 downto 0);
  signal jsfjfkrejc : real;
  signal jqp : std_logic_vector(3 to 3);
  signal wnzrux : real;
  signal ejbluqu : std_logic_vector(4 downto 0);
  signal ibynlrlf : std_logic_vector(3 to 3);
  signal a : real;
begin
  cpnuf : entity work.jjfl
    port map (sppednpa => a, ocgelx => ibynlrlf, vd => ejbluqu);
  pqhpmx : entity work.jjfl
    port map (sppednpa => wnzrux, ocgelx => jqp, vd => ejbluqu);
  zal : entity work.jjfl
    port map (sppednpa => jsfjfkrejc, ocgelx => ibynlrlf, vd => ufz);
  dt : entity work.jjfl
    port map (sppednpa => ieyizvmu, ocgelx => ibynlrlf, vd => vtqa);
  
  -- Single-driven assignments
  xbbaxyiwx <= xbbaxyiwx;
  
  -- Multi-driven assignments
  ibynlrlf <= "H";
  ibynlrlf <= (others => '0');
  jqp <= "Z";
end wyzz;



-- Seed after: 14955381691815092565,5511103086789671269
