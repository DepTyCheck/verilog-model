-- Seed: 17089040878707582457,3687118713772291287

entity xgmc is
  port (sbkyww : inout real; esuac : out time);
end xgmc;

architecture ddexyhhh of xgmc is
  
begin
  
end ddexyhhh;

library ieee;
use ieee.std_logic_1164.all;

entity mznmiln is
  port (ijqydvrun : in real; trrhhliv : in std_logic_vector(1 downto 1); wnaid : in std_logic_vector(4 to 2); c : buffer boolean_vector(2 downto 2));
end mznmiln;

architecture hcoy of mznmiln is
  
begin
  -- Single-driven assignments
  c <= (others => TRUE);
end hcoy;

entity qvzpmeeu is
  port (enqws : in integer; diln : in bit; jwwzqpg : out integer_vector(1 to 3));
end qvzpmeeu;

architecture zbfuyrwof of qvzpmeeu is
  signal tr : time;
  signal bahof : real;
  signal slljjvfjgn : time;
  signal dmtl : real;
  signal u : time;
  signal apkwhndca : real;
  signal ahwttye : time;
  signal spyky : real;
begin
  jwobsid : entity work.xgmc
    port map (sbkyww => spyky, esuac => ahwttye);
  kszplfxc : entity work.xgmc
    port map (sbkyww => apkwhndca, esuac => u);
  dc : entity work.xgmc
    port map (sbkyww => dmtl, esuac => slljjvfjgn);
  rrvygevol : entity work.xgmc
    port map (sbkyww => bahof, esuac => tr);
  
  -- Single-driven assignments
  jwwzqpg <= (16#837#, 16#5_9#, 3_0_4_0_2);
end zbfuyrwof;

library ieee;
use ieee.std_logic_1164.all;

entity zkkzu is
  port (t : in integer; j : in std_logic_vector(1 to 1); xooikij : in integer; xrppiushm : out std_logic_vector(0 to 3));
end zkkzu;

architecture wes of zkkzu is
  signal cbvmazt : time;
  signal laklm : real;
  signal wbxkf : time;
  signal zbsxjipqdy : real;
  signal cbvfiwcbvv : integer_vector(1 to 3);
  signal jdblfnhejy : bit;
  signal juqkunq : integer;
begin
  g : entity work.qvzpmeeu
    port map (enqws => juqkunq, diln => jdblfnhejy, jwwzqpg => cbvfiwcbvv);
  bapukcm : entity work.xgmc
    port map (sbkyww => zbsxjipqdy, esuac => wbxkf);
  trpifh : entity work.xgmc
    port map (sbkyww => laklm, esuac => cbvmazt);
  
  -- Single-driven assignments
  jdblfnhejy <= '0';
  juqkunq <= 3;
  
  -- Multi-driven assignments
  xrppiushm <= ('L', 'W', 'W', 'X');
end wes;



-- Seed after: 805605409541404573,3687118713772291287
