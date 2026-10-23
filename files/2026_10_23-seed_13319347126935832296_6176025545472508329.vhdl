-- Seed: 13319347126935832296,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity wtsei is
  port (fbnicyxx : in std_logic; ha : buffer std_logic_vector(3 to 0); itwhmbc : buffer integer_vector(4 to 1));
end wtsei;

architecture lppxg of wtsei is
  
begin
  -- Single-driven assignments
  itwhmbc <= (others => 0);
  
  -- Multi-driven assignments
  ha <= ha;
  ha <= ha;
  ha <= ha;
  ha <= ha;
end lppxg;

library ieee;
use ieee.std_logic_1164.all;

entity nkodedxyea is
  port (nb : buffer std_logic);
end nkodedxyea;

library ieee;
use ieee.std_logic_1164.all;

architecture vkpwkbc of nkodedxyea is
  signal tlg : integer_vector(4 to 1);
  signal cpgvh : std_logic;
  signal xqmpwycblu : integer_vector(4 to 1);
  signal wj : std_logic_vector(3 to 0);
  signal sdklag : std_logic;
  signal hezknkq : integer_vector(4 to 1);
  signal jtjhip : std_logic_vector(3 to 0);
  signal uozzny : integer_vector(4 to 1);
  signal ixffvljzbu : std_logic_vector(3 to 0);
begin
  qsloa : entity work.wtsei
    port map (fbnicyxx => nb, ha => ixffvljzbu, itwhmbc => uozzny);
  bbs : entity work.wtsei
    port map (fbnicyxx => nb, ha => jtjhip, itwhmbc => hezknkq);
  bcxrox : entity work.wtsei
    port map (fbnicyxx => sdklag, ha => wj, itwhmbc => xqmpwycblu);
  uvunjubbk : entity work.wtsei
    port map (fbnicyxx => cpgvh, ha => jtjhip, itwhmbc => tlg);
  
  -- Multi-driven assignments
  wj <= (others => '0');
  nb <= nb;
end vkpwkbc;



-- Seed after: 16509719696174709867,6176025545472508329
