-- Seed: 10131213478751022139,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity dfwjs is
  port (enkfmqdg : in std_logic; hrkkqhet : in time_vector(0 downto 0));
end dfwjs;

architecture hif of dfwjs is
  
begin
  
end hif;

entity tgc is
  port (dvyzr : buffer integer; zusfvzou : buffer bit_vector(4 to 0); iqyntnil : in time);
end tgc;

library ieee;
use ieee.std_logic_1164.all;

architecture ag of tgc is
  signal orqqws : std_logic;
  signal pueqmeg : time_vector(0 downto 0);
  signal mrwbt : time_vector(0 downto 0);
  signal tzcwbbdsfi : std_logic;
begin
  armua : entity work.dfwjs
    port map (enkfmqdg => tzcwbbdsfi, hrkkqhet => mrwbt);
  flsguzjfy : entity work.dfwjs
    port map (enkfmqdg => tzcwbbdsfi, hrkkqhet => pueqmeg);
  jzjq : entity work.dfwjs
    port map (enkfmqdg => orqqws, hrkkqhet => mrwbt);
  
  -- Single-driven assignments
  zusfvzou <= zusfvzou;
  mrwbt <= (others => 8#7# ps);
  dvyzr <= 441;
  pueqmeg <= (others => 16#F_B# ms);
  
  -- Multi-driven assignments
  tzcwbbdsfi <= 'U';
  orqqws <= '0';
  orqqws <= 'H';
  orqqws <= tzcwbbdsfi;
end ag;



-- Seed after: 152992030330275322,8437298063418820479
