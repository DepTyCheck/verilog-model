-- Seed: 13038171009678927365,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity cciorj is
  port (avidp : out real; poqnyo : out std_logic; zzbiybcygt : out std_logic_vector(4 downto 2));
end cciorj;

architecture qemmk of cciorj is
  
begin
  -- Single-driven assignments
  avidp <= 16#F_1_E_B.A_5_C_8#;
  
  -- Multi-driven assignments
  zzbiybcygt <= ('1', 'H', 'L');
  poqnyo <= 'Z';
end qemmk;

library ieee;
use ieee.std_logic_1164.all;

entity meyetjgg is
  port (cspnllc : in integer_vector(1 to 0); jgpnngmi : buffer std_logic; xumbrcp : inout real);
end meyetjgg;

library ieee;
use ieee.std_logic_1164.all;

architecture nrlej of meyetjgg is
  signal z : std_logic_vector(4 downto 2);
  signal hnlfpslmkm : std_logic;
  signal nrcjqm : real;
begin
  arkec : entity work.cciorj
    port map (avidp => nrcjqm, poqnyo => hnlfpslmkm, zzbiybcygt => z);
  rbphebutqo : entity work.cciorj
    port map (avidp => xumbrcp, poqnyo => hnlfpslmkm, zzbiybcygt => z);
  
  -- Multi-driven assignments
  hnlfpslmkm <= 'Z';
end nrlej;

library ieee;
use ieee.std_logic_1164.all;

entity wborycccd is
  port (mapvhi : in std_logic; ynryibzw : out std_logic_vector(1 to 2); nnynoowo : out integer_vector(4 to 4));
end wborycccd;

architecture ybieplne of wborycccd is
  
begin
  -- Single-driven assignments
  nnynoowo <= (others => 42);
  
  -- Multi-driven assignments
  ynryibzw <= "01";
  ynryibzw <= ('Z', '0');
  ynryibzw <= ('0', '1');
  ynryibzw <= "HW";
end ybieplne;

entity c is
  port (doqiqu : out character; peh : in integer_vector(3 to 2));
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture ncsmpp of c is
  signal hnkeci : real;
  signal gdkrko : std_logic;
  signal hkswgjchy : integer_vector(1 to 0);
  signal i : real;
  signal a : std_logic;
  signal dhsgky : real;
  signal agftle : std_logic;
begin
  rmrg : entity work.meyetjgg
    port map (cspnllc => peh, jgpnngmi => agftle, xumbrcp => dhsgky);
  z : entity work.meyetjgg
    port map (cspnllc => peh, jgpnngmi => a, xumbrcp => i);
  iesapoe : entity work.meyetjgg
    port map (cspnllc => hkswgjchy, jgpnngmi => gdkrko, xumbrcp => hnkeci);
  
  -- Single-driven assignments
  doqiqu <= 'h';
  hkswgjchy <= (others => 0);
  
  -- Multi-driven assignments
  agftle <= 'Z';
  gdkrko <= 'H';
  agftle <= 'L';
end ncsmpp;



-- Seed after: 7215578228174529046,1834764876137802293
