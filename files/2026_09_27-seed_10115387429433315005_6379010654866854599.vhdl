-- Seed: 10115387429433315005,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (bpstmi : buffer time; j : inout real; y : buffer string(4 to 1); epkck : inout std_logic_vector(2 downto 4));
end t;

architecture e of t is
  
begin
  -- Single-driven assignments
  bpstmi <= bpstmi;
  y <= y;
  
  -- Multi-driven assignments
  epkck <= "";
  epkck <= (others => '0');
  epkck <= "";
  epkck <= (others => '0');
end e;

entity vcpxlmq is
  port (bneiokq : linkage integer; sxb : buffer real);
end vcpxlmq;

library ieee;
use ieee.std_logic_1164.all;

architecture w of vcpxlmq is
  signal vedpfgow : string(4 to 1);
  signal lfwbq : time;
  signal qms : std_logic_vector(2 downto 4);
  signal ukfqqmd : string(4 to 1);
  signal uitsgvg : real;
  signal caqwfvjb : time;
begin
  sn : entity work.t
    port map (bpstmi => caqwfvjb, j => uitsgvg, y => ukfqqmd, epkck => qms);
  idy : entity work.t
    port map (bpstmi => lfwbq, j => sxb, y => vedpfgow, epkck => qms);
  
  -- Multi-driven assignments
  qms <= qms;
  qms <= "";
  qms <= qms;
end w;

entity lbjupfcr is
  port (kulsop : in time; yl : out boolean; ppyx : in string(1 downto 4));
end lbjupfcr;

library ieee;
use ieee.std_logic_1164.all;

architecture pflu of lbjupfcr is
  signal hia : std_logic_vector(2 downto 4);
  signal zqdvrsgqfq : string(4 to 1);
  signal dqcj : real;
  signal fgnt : time;
begin
  ekewkhko : entity work.t
    port map (bpstmi => fgnt, j => dqcj, y => zqdvrsgqfq, epkck => hia);
  
  -- Single-driven assignments
  yl <= FALSE;
  
  -- Multi-driven assignments
  hia <= hia;
end pflu;

library ieee;
use ieee.std_logic_1164.all;

entity opnvwuib is
  port (a : buffer real_vector(3 downto 3); vfigbm : buffer std_logic_vector(3 downto 0); zbquyt : in character; gumbxbot : in character);
end opnvwuib;

architecture amhxxja of opnvwuib is
  signal sywciyg : string(1 downto 4);
  signal djqeidim : boolean;
  signal fdtmyu : time;
begin
  drfjm : entity work.lbjupfcr
    port map (kulsop => fdtmyu, yl => djqeidim, ppyx => sywciyg);
  
  -- Single-driven assignments
  fdtmyu <= 8#2.1# us;
  a <= a;
  sywciyg <= "";
  
  -- Multi-driven assignments
  vfigbm <= vfigbm;
end amhxxja;



-- Seed after: 10590977672568567739,6379010654866854599
