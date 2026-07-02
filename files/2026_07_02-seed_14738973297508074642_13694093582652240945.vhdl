-- Seed: 14738973297508074642,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity rvb is
  port (nppowgau : buffer std_logic; k : out severity_level; zidchijgi : linkage integer_vector(2 downto 3); qinogc : buffer integer);
end rvb;

architecture dxlqnoidhz of rvb is
  
begin
  -- Single-driven assignments
  k <= WARNING;
  qinogc <= 8#011#;
  
  -- Multi-driven assignments
  nppowgau <= 'Z';
  nppowgau <= 'W';
end dxlqnoidhz;

entity ycsmufdw is
  port (pa : in time);
end ycsmufdw;

library ieee;
use ieee.std_logic_1164.all;

architecture ya of ycsmufdw is
  signal tf : integer;
  signal icxnfbq : integer_vector(2 downto 3);
  signal tqhip : severity_level;
  signal w : std_logic;
begin
  tjvoyj : entity work.rvb
    port map (nppowgau => w, k => tqhip, zidchijgi => icxnfbq, qinogc => tf);
  
  -- Multi-driven assignments
  w <= 'Z';
end ya;

library ieee;
use ieee.std_logic_1164.all;

entity spksp is
  port (ygpe : out std_logic_vector(3 downto 3); tu : linkage std_logic_vector(0 to 0));
end spksp;

library ieee;
use ieee.std_logic_1164.all;

architecture fdvrm of spksp is
  signal txydqd : integer;
  signal vmkrjgamew : integer_vector(2 downto 3);
  signal tbh : severity_level;
  signal jvijongq : integer;
  signal gpdpfwaib : integer_vector(2 downto 3);
  signal o : severity_level;
  signal dobuxkt : std_logic;
  signal vqjafd : time;
begin
  tyn : entity work.ycsmufdw
    port map (pa => vqjafd);
  gfmnvop : entity work.rvb
    port map (nppowgau => dobuxkt, k => o, zidchijgi => gpdpfwaib, qinogc => jvijongq);
  ss : entity work.rvb
    port map (nppowgau => dobuxkt, k => tbh, zidchijgi => vmkrjgamew, qinogc => txydqd);
  
  -- Single-driven assignments
  vqjafd <= 4_4_0_2_3.3_4 ms;
  
  -- Multi-driven assignments
  dobuxkt <= '0';
end fdvrm;



-- Seed after: 12684437608663836453,13694093582652240945
