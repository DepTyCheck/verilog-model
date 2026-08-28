-- Seed: 14820039236175580829,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity xtwmpay is
  port (txsbdgdtd : inout std_logic; jc : linkage time);
end xtwmpay;

architecture ky of xtwmpay is
  
begin
  -- Multi-driven assignments
  txsbdgdtd <= 'U';
end ky;

library ieee;
use ieee.std_logic_1164.all;

entity iwtxr is
  port (cznnrk : linkage std_logic_vector(2 to 2));
end iwtxr;

library ieee;
use ieee.std_logic_1164.all;

architecture uyix of iwtxr is
  signal pantwbcowb : time;
  signal shstjphp : time;
  signal fasp : std_logic;
  signal eyapweqswh : time;
  signal qptl : std_logic;
begin
  wxbgaa : entity work.xtwmpay
    port map (txsbdgdtd => qptl, jc => eyapweqswh);
  qjqh : entity work.xtwmpay
    port map (txsbdgdtd => fasp, jc => shstjphp);
  utwit : entity work.xtwmpay
    port map (txsbdgdtd => qptl, jc => pantwbcowb);
  
  -- Multi-driven assignments
  fasp <= '0';
  fasp <= qptl;
  qptl <= 'H';
  qptl <= fasp;
end uyix;



-- Seed after: 15686249637223491118,7198033922882419595
