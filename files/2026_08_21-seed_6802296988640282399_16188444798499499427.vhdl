-- Seed: 6802296988640282399,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity iwsvk is
  port (x : inout std_logic; a : in time; di : buffer time);
end iwsvk;

architecture pr of iwsvk is
  
begin
  -- Single-driven assignments
  di <= 330 ms;
  
  -- Multi-driven assignments
  x <= x;
  x <= x;
  x <= 'Z';
  x <= '-';
end pr;

entity qkjzhoxm is
  port (mdapd : buffer integer; whjiewedr : buffer real_vector(0 to 0); vizhtd : inout boolean; nfosxy : linkage time);
end qkjzhoxm;

library ieee;
use ieee.std_logic_1164.all;

architecture wxmjh of qkjzhoxm is
  signal qhjicie : std_logic;
  signal yiblgic : time;
  signal wb : time;
  signal tqsrgcjhe : time;
  signal vkqqxmrrvb : std_logic;
begin
  faddyecya : entity work.iwsvk
    port map (x => vkqqxmrrvb, a => tqsrgcjhe, di => tqsrgcjhe);
  nl : entity work.iwsvk
    port map (x => vkqqxmrrvb, a => wb, di => yiblgic);
  lcltjobct : entity work.iwsvk
    port map (x => qhjicie, a => yiblgic, di => wb);
  
  -- Single-driven assignments
  vizhtd <= vizhtd;
  whjiewedr <= whjiewedr;
  mdapd <= mdapd;
  
  -- Multi-driven assignments
  vkqqxmrrvb <= '0';
  vkqqxmrrvb <= 'H';
end wxmjh;

entity ld is
  port (audli : inout real_vector(3 to 4));
end ld;

library ieee;
use ieee.std_logic_1164.all;

architecture xiydbqd of ld is
  signal en : time;
  signal ahqxpydfya : std_logic;
  signal cdi : time;
  signal oisipniwbh : time;
  signal bxpbyq : boolean;
  signal blinrv : real_vector(0 to 0);
  signal lm : integer;
  signal utcejbi : time;
  signal inzkexir : time;
  signal z : std_logic;
begin
  ykejnnifqm : entity work.iwsvk
    port map (x => z, a => inzkexir, di => utcejbi);
  jpwq : entity work.qkjzhoxm
    port map (mdapd => lm, whjiewedr => blinrv, vizhtd => bxpbyq, nfosxy => inzkexir);
  guo : entity work.iwsvk
    port map (x => z, a => oisipniwbh, di => cdi);
  habkzagwf : entity work.iwsvk
    port map (x => ahqxpydfya, a => en, di => en);
end xiydbqd;

library ieee;
use ieee.std_logic_1164.all;

entity ceghhq is
  port (dqewcextr : buffer character; yzhkwqftcr : in character; mphwazhrl : inout boolean; f : in std_logic_vector(2 to 2));
end ceghhq;

library ieee;
use ieee.std_logic_1164.all;

architecture vqwtdj of ceghhq is
  signal ela : time;
  signal qfebkrk : std_logic;
  signal llzkz : time;
  signal bqvhy : boolean;
  signal gunhydle : real_vector(0 to 0);
  signal sr : integer;
begin
  kixuefg : entity work.qkjzhoxm
    port map (mdapd => sr, whjiewedr => gunhydle, vizhtd => bqvhy, nfosxy => llzkz);
  sqxkdtc : entity work.iwsvk
    port map (x => qfebkrk, a => llzkz, di => ela);
  
  -- Single-driven assignments
  dqewcextr <= yzhkwqftcr;
  mphwazhrl <= TRUE;
end vqwtdj;



-- Seed after: 16412086329517407867,16188444798499499427
