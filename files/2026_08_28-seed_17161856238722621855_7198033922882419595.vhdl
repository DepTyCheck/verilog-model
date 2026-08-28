-- Seed: 17161856238722621855,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity yatia is
  port (ou : buffer std_logic; ybmrh : inout string(5 downto 3));
end yatia;

architecture dzyradlal of yatia is
  
begin
  -- Single-driven assignments
  ybmrh <= "pcu";
  
  -- Multi-driven assignments
  ou <= '0';
end dzyradlal;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (u : out integer_vector(0 to 3); jxqxgkqmpt : linkage std_logic; acdscaqm : linkage real; jb : out real_vector(2 to 3));
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture lj of t is
  signal gzedg : string(5 downto 3);
  signal pdnqf : string(5 downto 3);
  signal om : std_logic;
begin
  qqfbdg : entity work.yatia
    port map (ou => om, ybmrh => pdnqf);
  lvs : entity work.yatia
    port map (ou => om, ybmrh => gzedg);
  
  -- Single-driven assignments
  u <= u;
end lj;



-- Seed after: 8377799375956580903,7198033922882419595
