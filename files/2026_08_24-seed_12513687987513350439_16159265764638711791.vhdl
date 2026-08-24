-- Seed: 12513687987513350439,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity uegowmvxu is
  port (kkoaawnpfj : inout std_logic);
end uegowmvxu;

architecture qxdntltsd of uegowmvxu is
  
begin
  -- Multi-driven assignments
  kkoaawnpfj <= kkoaawnpfj;
  kkoaawnpfj <= 'U';
  kkoaawnpfj <= 'W';
  kkoaawnpfj <= 'X';
end qxdntltsd;

library ieee;
use ieee.std_logic_1164.all;

entity xzlbwjxv is
  port (ptvd : buffer time_vector(2 to 4); tyeo : buffer std_logic);
end xzlbwjxv;

library ieee;
use ieee.std_logic_1164.all;

architecture aq of xzlbwjxv is
  signal bbut : std_logic;
begin
  r : entity work.uegowmvxu
    port map (kkoaawnpfj => tyeo);
  pny : entity work.uegowmvxu
    port map (kkoaawnpfj => bbut);
end aq;

library ieee;
use ieee.std_logic_1164.all;

entity cqoumrp is
  port (bklrl : buffer bit; vva : inout std_logic);
end cqoumrp;

library ieee;
use ieee.std_logic_1164.all;

architecture jicnpvdf of cqoumrp is
  signal cjl : std_logic;
begin
  ucxvtkg : entity work.uegowmvxu
    port map (kkoaawnpfj => cjl);
end jicnpvdf;

entity p is
  port (rmfncptwq : linkage bit; snwq : out integer);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture b of p is
  signal ko : std_logic;
  signal cljb : std_logic;
  signal sbzxogi : time_vector(2 to 4);
begin
  ulnxolp : entity work.xzlbwjxv
    port map (ptvd => sbzxogi, tyeo => cljb);
  wjtezwaf : entity work.uegowmvxu
    port map (kkoaawnpfj => cljb);
  ckpc : entity work.uegowmvxu
    port map (kkoaawnpfj => ko);
  
  -- Multi-driven assignments
  ko <= 'Z';
  ko <= '-';
  cljb <= cljb;
  ko <= cljb;
end b;



-- Seed after: 3373796917667510953,16159265764638711791
