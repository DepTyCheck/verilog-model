-- Seed: 15654183545896465417,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity pchqbiuj is
  port (wyi : in bit; pt : buffer std_logic_vector(3 to 2));
end pchqbiuj;

architecture hq of pchqbiuj is
  
begin
  -- Multi-driven assignments
  pt <= "";
  pt <= (others => '0');
  pt <= (others => '0');
  pt <= (others => '0');
end hq;

library ieee;
use ieee.std_logic_1164.all;

entity pzkpr is
  port (hiuao : linkage boolean; kioytxoaoq : in integer; jpceoaphg : out std_logic_vector(3 to 4));
end pzkpr;

library ieee;
use ieee.std_logic_1164.all;

architecture tvoaycmj of pzkpr is
  signal cs : std_logic_vector(3 to 2);
  signal ihmp : bit;
begin
  xelxt : entity work.pchqbiuj
    port map (wyi => ihmp, pt => cs);
  
  -- Single-driven assignments
  ihmp <= '0';
  
  -- Multi-driven assignments
  cs <= "";
  cs <= "";
  jpceoaphg <= "WW";
end tvoaycmj;

library ieee;
use ieee.std_logic_1164.all;

entity vthljq is
  port (ct : inout integer; wckifnp : inout boolean; zyuu : out std_logic; vseiymf : linkage real);
end vthljq;

library ieee;
use ieee.std_logic_1164.all;

architecture neyxbko of vthljq is
  signal ewbzqy : std_logic_vector(3 to 4);
  signal dncpwkp : std_logic_vector(3 to 2);
  signal fugicgyr : bit;
  signal csgrj : bit;
  signal zotupz : std_logic_vector(3 to 2);
  signal fubmftk : bit;
begin
  xaiiyawrb : entity work.pchqbiuj
    port map (wyi => fubmftk, pt => zotupz);
  bogwzqfm : entity work.pchqbiuj
    port map (wyi => csgrj, pt => zotupz);
  grlp : entity work.pchqbiuj
    port map (wyi => fugicgyr, pt => dncpwkp);
  njbl : entity work.pzkpr
    port map (hiuao => wckifnp, kioytxoaoq => ct, jpceoaphg => ewbzqy);
  
  -- Single-driven assignments
  fugicgyr <= '1';
end neyxbko;



-- Seed after: 11268361366703647009,13479070923501788437
