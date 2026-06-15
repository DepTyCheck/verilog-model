-- Seed: 6986176530640909487,15300320181035395489

entity r is
  port (dslj : inout character);
end r;

architecture njg of r is
  
begin
  -- Single-driven assignments
  dslj <= 'k';
end njg;

library ieee;
use ieee.std_logic_1164.all;

entity rl is
  port (ghfba : buffer time; jjtkccpq : buffer std_logic_vector(4 to 2); qzkp : buffer std_logic_vector(1 to 0));
end rl;

architecture lx of rl is
  signal nug : character;
  signal yg : character;
  signal mdnpuagc : character;
  signal yzcxd : character;
begin
  qkcmuh : entity work.r
    port map (dslj => yzcxd);
  bpkodxvqci : entity work.r
    port map (dslj => mdnpuagc);
  hett : entity work.r
    port map (dslj => yg);
  juxchvr : entity work.r
    port map (dslj => nug);
  
  -- Multi-driven assignments
  qzkp <= (others => '0');
end lx;

library ieee;
use ieee.std_logic_1164.all;

entity mabzma is
  port (fmopel : inout std_logic_vector(3 downto 3); n : inout time; mqgtw : inout integer);
end mabzma;

architecture rcz of mabzma is
  signal xqtrc : character;
begin
  f : entity work.r
    port map (dslj => xqtrc);
  
  -- Single-driven assignments
  mqgtw <= 4_1;
  n <= 022 ms;
  
  -- Multi-driven assignments
  fmopel <= (others => 'X');
end rcz;

library ieee;
use ieee.std_logic_1164.all;

entity qz is
  port (hjoeco : out std_logic; kozj : inout boolean; mempttbwl : buffer integer; bocazcc : buffer time_vector(2 downto 4));
end qz;

library ieee;
use ieee.std_logic_1164.all;

architecture mrzly of qz is
  signal wlv : std_logic_vector(1 to 0);
  signal itjxr : time;
  signal uliejrj : character;
begin
  za : entity work.r
    port map (dslj => uliejrj);
  z : entity work.rl
    port map (ghfba => itjxr, jjtkccpq => wlv, qzkp => wlv);
  
  -- Single-driven assignments
  mempttbwl <= 3_2_1_3;
  kozj <= FALSE;
  bocazcc <= (others => 0 ns);
end mrzly;



-- Seed after: 440384432047883309,15300320181035395489
