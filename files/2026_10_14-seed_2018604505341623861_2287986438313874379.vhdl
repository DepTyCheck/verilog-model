-- Seed: 2018604505341623861,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity keortnbsv is
  port (qldcehm : out character; kimctphwz : out boolean; gireqqunbb : inout std_logic_vector(1 to 1); g : linkage std_logic);
end keortnbsv;

architecture ugygpjya of keortnbsv is
  
begin
  -- Single-driven assignments
  kimctphwz <= FALSE;
  qldcehm <= qldcehm;
  
  -- Multi-driven assignments
  gireqqunbb <= gireqqunbb;
  gireqqunbb <= gireqqunbb;
  gireqqunbb <= (others => 'X');
  gireqqunbb <= gireqqunbb;
end ugygpjya;

entity wyesnlh is
  port (kes : out severity_level; aamjx : out severity_level; mjfs : inout time);
end wyesnlh;

library ieee;
use ieee.std_logic_1164.all;

architecture bimk of wyesnlh is
  signal h : boolean;
  signal oyxbubg : character;
  signal lvbybjd : std_logic;
  signal xnme : std_logic_vector(1 to 1);
  signal tbizavxrdp : boolean;
  signal hu : character;
  signal mtmhuo : std_logic;
  signal j : std_logic_vector(1 to 1);
  signal enkszazg : boolean;
  signal unwlx : character;
begin
  vlcwuqu : entity work.keortnbsv
    port map (qldcehm => unwlx, kimctphwz => enkszazg, gireqqunbb => j, g => mtmhuo);
  orokl : entity work.keortnbsv
    port map (qldcehm => hu, kimctphwz => tbizavxrdp, gireqqunbb => xnme, g => lvbybjd);
  wl : entity work.keortnbsv
    port map (qldcehm => oyxbubg, kimctphwz => h, gireqqunbb => j, g => lvbybjd);
  
  -- Single-driven assignments
  kes <= WARNING;
  mjfs <= mjfs;
  aamjx <= FAILURE;
end bimk;



-- Seed after: 13692858426451344705,2287986438313874379
