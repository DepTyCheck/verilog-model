-- Seed: 17721992518980464767,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity obacxfeokk is
  port (orswjpes : linkage std_logic_vector(0 downto 4); idp : inout integer_vector(0 to 2));
end obacxfeokk;

architecture zjtfs of obacxfeokk is
  
begin
  -- Single-driven assignments
  idp <= idp;
end zjtfs;

library ieee;
use ieee.std_logic_1164.all;

entity jrcc is
  port (jeup : inout std_logic);
end jrcc;

library ieee;
use ieee.std_logic_1164.all;

architecture hkcahexx of jrcc is
  signal tm : integer_vector(0 to 2);
  signal ixzsjvzzam : integer_vector(0 to 2);
  signal pxpptryz : integer_vector(0 to 2);
  signal ilb : std_logic_vector(0 downto 4);
  signal ymaaikov : integer_vector(0 to 2);
  signal dtyuoes : std_logic_vector(0 downto 4);
begin
  bo : entity work.obacxfeokk
    port map (orswjpes => dtyuoes, idp => ymaaikov);
  npapqi : entity work.obacxfeokk
    port map (orswjpes => ilb, idp => pxpptryz);
  d : entity work.obacxfeokk
    port map (orswjpes => dtyuoes, idp => ixzsjvzzam);
  mp : entity work.obacxfeokk
    port map (orswjpes => dtyuoes, idp => tm);
  
  -- Multi-driven assignments
  ilb <= dtyuoes;
  jeup <= 'Z';
  jeup <= jeup;
end hkcahexx;



-- Seed after: 6084008658813674805,10871023049702252113
