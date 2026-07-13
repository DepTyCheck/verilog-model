-- Seed: 9645094978449762075,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity qhkw is
  port (rxrm : linkage std_logic_vector(2 downto 0); mm : inout protected_subtype_mirror; aun : linkage std_logic);
end qhkw;

architecture ldyp of qhkw is
  
begin
  
end ldyp;

entity kenmay is
  port (znqcs : inout string(1 downto 3));
end kenmay;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture wwnj of kenmay is
  shared variable dk : protected_subtype_mirror;
  shared variable waayzahla : protected_subtype_mirror;
  signal cfsnwfsu : std_logic;
  shared variable sk : protected_subtype_mirror;
  signal oyqv : std_logic_vector(2 downto 0);
begin
  gx : entity work.qhkw
    port map (rxrm => oyqv, mm => sk, aun => cfsnwfsu);
  uuln : entity work.qhkw
    port map (rxrm => oyqv, mm => waayzahla, aun => cfsnwfsu);
  yin : entity work.qhkw
    port map (rxrm => oyqv, mm => dk, aun => cfsnwfsu);
  
  -- Multi-driven assignments
  cfsnwfsu <= cfsnwfsu;
  cfsnwfsu <= 'H';
  oyqv <= ('0', 'X', '0');
end wwnj;

entity l is
  port (hzxvszdrm : linkage real; wkxvqxbiah : in real; tsfi : linkage bit_vector(1 downto 4));
end l;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture cvhzaycp of l is
  shared variable mqabjsry : protected_subtype_mirror;
  signal za : std_logic_vector(2 downto 0);
  shared variable eitzeaxoj : protected_subtype_mirror;
  signal rp : std_logic;
  shared variable ylydknwl : protected_subtype_mirror;
  signal jvi : std_logic_vector(2 downto 0);
  signal zristwbdvr : string(1 downto 3);
begin
  mmsxj : entity work.kenmay
    port map (znqcs => zristwbdvr);
  sjhr : entity work.qhkw
    port map (rxrm => jvi, mm => ylydknwl, aun => rp);
  aohy : entity work.qhkw
    port map (rxrm => jvi, mm => eitzeaxoj, aun => rp);
  obxiltpziu : entity work.qhkw
    port map (rxrm => za, mm => mqabjsry, aun => rp);
  
  -- Multi-driven assignments
  jvi <= jvi;
  jvi <= ('0', 'H', '1');
  jvi <= jvi;
end cvhzaycp;



-- Seed after: 12324450187536001960,3566912872917928779
